{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Chat
Description : Chat module for Snap Framework
Author      : Dennis J. McWherter, Jr.
Maintainer  : dennis@deathbytape.com
Stability   : experimental
Portability : POSIX

Chat Snaplet used in example tutorial.
-}
module Snap.Snaplet.Chat(
  Chat(..)
  ,initChat
  ) where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Exception (catch)
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Class
import Data.ByteString
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Read
import Data.Time.LocalTime
import qualified Database.PostgreSQL.Simple as PGS
import Network.WebSockets
import Network.WebSockets.Snap
import Opaleye
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple (Postgres(..), liftPG, withPG)

type UserIdentity = (LBS.ByteString, Int)
type BroadcastChannel = (Chan LBS.ByteString)

data Chat = Chat { bcastChan :: BroadcastChannel -- ^ Broadcast channel
                 , userCount :: IORef Int -- ^ Current user count
                 , _db :: Snaplet Postgres -- ^ Postgres db connection
--                 , _auth :: Snaplet (AuthManager b) -- ^ Auth manager
                 }

makeLenses ''Chat

data ChatMessage' a b c d = ChatMessage' { msgId :: a -- ^ Unique identifier for message
                                         , msgText :: b -- ^ Message text
                                         , msgUserId :: c -- ^ User id of poster
                                         , msgDate :: d -- ^ Date message was posted
                                         }
type ChatMessage = ChatMessage' Int String Int LocalTime
type ChatColumnW = ChatMessage' (Maybe (Column PGInt4)) (Column PGText) (Column PGInt4) (Maybe (Column PGTimestamp))
type ChatColumnR = ChatMessage' (Column PGInt4) (Column PGText) (Column PGInt4) (Column PGTimestamp)

-- Nice display format
instance (Show b, Show c, Show d) => Show (ChatMessage' a b c d) where
  show (ChatMessage' _ txt user date) = "[" ++ show date ++ "] <" ++ show user ++ "> " ++ show txt

-- Generate profunctor
$(makeAdaptorAndInstance "pChatMessage" ''ChatMessage')

-- | Table definition for chat messages
chatMessageTable :: Table ChatColumnW ChatColumnR
chatMessageTable = Table "chat"
                   (pChatMessage ChatMessage' { msgId = optional "id"
                                              , msgText = required "message"
                                              , msgUserId = required "user_id"
                                              , msgDate = optional "date"
                                              })

-- | Websocket server for real-time chat communication
chatServer :: UserIdentity -> Handler b Chat ()
chatServer (user, uid) = do
  bchan <- gets bcastChan
  cntRef <- gets userCount
  dbSnaplet <- gets _db
  liftIO $ incCount cntRef
  runWebSocketsSnap $ (flip catch) (handleQuit cntRef) . handler bchan dbSnaplet
  where -- Helper to accept client connection request and setup serving loop
        handler :: BroadcastChannel -> Snaplet Postgres -> ServerApp
        handler chan dbSnaplet pconn = do
          conn <- acceptRequest pconn
          dup <- dupChan chan
          forkPingThread conn 15 -- Check that our user is alive every 15 seconds
          wsReader <- async $ readChan dup
          wsWriter <- async $ receiveDataMessage conn
          serve conn dbSnaplet dup wsReader wsWriter
        -- Helper to actually manages comm among users
        serve :: Connection -> Snaplet Postgres -> BroadcastChannel -> Async LBS.ByteString -> Async DataMessage -> IO ()
        serve conn dbSnaplet chan wsReader wsWriter = do
          result <- waitEither wsReader wsWriter
          case result of
           Left msg -> sendDataMessage conn $ Text msg
           Right (Text msg) -> do
             written <- runReaderT (withPG $ liftPG $ storeMessage msg) dbSnaplet
             Prelude.putStrLn $ if written > 0 then "stored message." else "did not store message."
             writeChan chan (LBS.append "<" $ LBS.append user $ LBS.append "> " msg)
           Right _ -> Prelude.putStrLn "Received some binary data from client. Ignoring."
          -- NOTE: This is ugly.. It continuously creates/tearsdown threads
          -- Determine who won the race and which async we need to restart
          let loop = serve conn dbSnaplet chan
          case result of
           Left _ -> do
             nextReader <- async $ readChan chan
             loop nextReader wsWriter
           Right _ -> do
             nextWriter <- async $ receiveDataMessage conn
             loop wsReader nextWriter
        -- Helper to handle when user quits
        handleQuit :: IORef Int -> ConnectionException -> IO ()
        handleQuit cntRef (CloseRequest _ _) = decCount cntRef
        handleQuit cntRef ConnectionClosed = decCount cntRef
        handleQuit _ e = Prelude.putStrLn $ "Unhandled exception: " ++ show e
        -- Helpers for modifying user count
        updateCount :: IORef Int -> (Int -> (Int, Int)) -> IO ()
        updateCount cntRef fn = atomicModifyIORef cntRef fn >>= Prelude.putStrLn . ("User count: " ++) . show 
        decCount cntRef = updateCount cntRef (\x -> let y = x - 1 in (y, y))
        incCount cntRef = updateCount cntRef (\x -> let y = x + 1 in (y, y))
        -- Database helper
        storeMessage msg conn = runInsert conn chatMessageTable $
            ChatMessage' { msgId = Nothing
                         , msgText = (pgString . C.unpack) msg
                         , msgUserId = pgInt4 uid
                         , msgDate = Nothing
                         }

-- | Handler responsible for displaying main chat page
pageHandler :: Handler b Chat ()
pageHandler = writeText "Send user to chat page."

-- | Handler to retrieve the last 50 chat messages
getLastFifty :: Handler b Postgres ()
getLastFifty = do
  msgs <- liftPG $ getMessages ((limit 50 . orderBy (desc msgDate) . queryTable) chatMessageTable)
  writeText $ if Prelude.length msgs > 0
              then T.append (T.append "<pre>" ((T.pack . Prelude.foldl ((++) . (++"\n"))  "" . fmap show) msgs)) "</pre>"
              else "No messages to display"
  where getMessages :: Query ChatColumnR -> PGS.Connection -> IO [ChatMessage]
        getMessages = flip runQuery

-- | Routes protected by login
routes :: SnapletLens b (AuthManager b) -> [(ByteString, Handler b Chat ())]
routes auth = (fmap $ enforceLogin auth)
         [ ("/", pageHandler)
         , ("/chat", handleChatClient)
         , ("/last", with db $ getLastFifty)]
  where handleChatClient = do
          user <- withTop auth $ do
            cur <- currentUser
            return $ case cur of
                      Just u ->
                        case userId u of
                         Just uid -> case (decimal . unUid) uid of
                           Right idVal -> Just (textToLBS $ userLogin u, fst idVal)
                           Left _ -> Nothing
                         Nothing -> Nothing
                      Nothing -> Nothing
          maybe (return ()) chatServer user
        textToLBS = LBS.pack . unpack . encodeUtf8
               
-- | Initialize snaplet by providing a snaplet containing an active
-- database connection.
initChat :: SnapletLens b (AuthManager b) -- ^ Auth manager
         -> Snaplet Postgres -- ^ Postgres DB
         -> SnapletInit b Chat
initChat auth db' = makeSnaplet "chat" "web chat backend" Nothing $ do
    addRoutes $ routes auth
    cnt <- liftIO $ newIORef 0
    chan <- liftIO $ newChan
    return $! Chat chan cnt db'

enforceLogin :: SnapletLens b (AuthManager b)
             -> (ByteString, Handler b v ())
             -> (ByteString, Handler b v ())
enforceLogin auth (uri, handler) = (uri, requireUser auth badLogin handler)
  where badLogin = do
          modifyResponse $ setResponseCode 401
          writeBS "Please login to access any chat features."

