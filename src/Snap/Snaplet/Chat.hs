{-# LANGUAGE OverloadedStrings #-}
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

import Data.ByteString
import Network.WebSockets
import Network.WebSockets.Snap
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth

data Chat = Chat {}

-- | Websocket server for real-time chat communication
chatServer :: Handler b Chat ()
chatServer = runWebSocketsSnap handler
  where handler :: ServerApp
        handler pconn = acceptRequest pconn >>= serve
        serve :: Connection -> IO ()
        serve conn = do
          msg <- receiveDataMessage conn
          case msg of
           Text str -> if str == "done"
                       then Prelude.putStrLn "Closing connection."
                       else (Prelude.putStrLn $ "Found: " ++ show str) >> serve conn
           _ -> Prelude.putStrLn "Received binary data? Throwing it away."

-- | Handler responsible for displaying main chat page
pageHandler :: Handler b Chat ()
pageHandler = writeText "Send user to chat page."

-- | Routes protected by login
routes :: SnapletLens b (AuthManager b) -> [(ByteString, Handler b Chat ())]
routes = (flip fmap)
         [ ("/", pageHandler)
         , ("/chat", chatServer)]
         . enforceLogin

-- | Initialize snaplet by providing a snaplet containing an active
-- database connection.
initChat :: SnapletLens b (AuthManager b) -- ^ Auth manager
         -> SnapletInit b Chat
initChat auth = makeSnaplet "chat" "web chat backend" Nothing $ do
    addRoutes $ routes auth
    return $ Chat 

enforceLogin :: SnapletLens b (AuthManager b)
             -> (ByteString, Handler b Chat ())
             -> (ByteString, Handler b Chat ())
enforceLogin auth (uri, handler) = (uri, requireUser auth badLogin handler)
  where badLogin = writeText "Not logged in. You cannot chat."


