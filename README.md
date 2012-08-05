DSLBOT
======

This is a Haskell EDSL around the ircbot package on hackage (http://hackage.haskell.org/package/ircbot-0.5.1). The aim of DSLBot is to provide a very simple to use Domain Specific Langiage for creating IRC bots.

How can i try it?
-----------------
As of this writing, DSLBot uses slighty modified variants of ircbot and irc, these can be found here: https://github.com/jonte/ircbot, and here: https://github.com/jonte/hsirc.
Unpacking and merging Network/ of ircbot and irc into the tree of DSLBot should provide a workable environment to test DSLBot in.

Why do I want it?
-----------------
You can write real snazzy-looking IRC bots, like this one:

    import DSLBot
    myBot :: DSL ()
    myBot = do
        set (nickname "myBot")
        set (server "irc.freenode.org")
        set (name "myBot")            
        join "#bottybotbot"          
        whenReceived "Whaddup" $ do
            respond "Not much" 
            respond "U?"                       
        whenReceived "Kick me" $ do
            respond "OK"
            kickUser currentChannel currentUser
        whenReceived "Would you leave, please?" $ do
            respond "Sure thing"
            partChannel currentChannel
        whenReceived "Give me ops" $ do
            respond "Giving ops"
            giveOps currentChannel currentUser
        whenReceived "Take ops" $ do
            takeOps currentChannel currentUser
        connect
        where currentUser = CurrentUser
              currentChannel = "#bottybotbot"
    
    main :: IO ()
    main = run myBot

What's the status of the project?
---------------------------------
The code is not yet usable, and several key features are missing. The above example should work however ;)
