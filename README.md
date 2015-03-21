Realtime Chat (WebSockets) with Snap
====================================

This is an example of creating a realtime chat server using WebSockets with the snap framework. This is accompanied by [a blog post](http://deathbytape.com/post/114214290439/haskell-snap-realtime-chat) that you can read for some basic guidance. It is intended simply as an example and is **NOT** intended for production use. There are very obvious _performance_ and _security_ concerns with this code. First of all, user input is not santized (i.e. clients may send malicious Javascript directly through chat) and the threading model is far from ideal.

I was learning some of these frameworks as I wrote this code, so it may not be the absolute best way (but, is there ever really such a thing?) to operate within the model provided by the frameworks, but you can certainly use this as a starting point for determing how things are to be done.

Building
========

To build and run this code, first create a table called "chat" in your PostgreSQL database. Then load the schema located in "bootstrap" into this table. From there, run the following commands:

```bash
cabal sandbox init
cabal install --dependencies-only
cabal run
```

Navigating
==========

All functionality is located at the /chat endpoint. This endpoint is protected by the auth system, so you must first create an account and login. After having done this, try visiting

[http://localhost:8000/chat](http://localhost:8000/chat)

Similarly, after you have chatted for a little bit of time, you can visit [http://localhost:8000/chat/last](http://localhost:8000/chat/last) to see a history of your last 50 messages (note: all messages are still in the DB, but this endpoint only displays 50).

