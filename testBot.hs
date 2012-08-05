import DSLBot
connectAndJoin :: DSL ()
connectAndJoin = do
    set (nickname "Jonte22")
    set (server "irc.freenode.org")
    set (name "Jonte")            
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
main = run connectAndJoin
