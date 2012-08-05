module DSLBot where
import Network.IRC.Bot.Core
import Data.Set
import Network.IRC.Bot.Log
import Network.IRC.Bot.Part.Ping
import Network.IRC.Bot.Part.Channels
import Network.IRC.Bot.BotMonad
import Network.IRC.Bot.Commands (PrivMsg(..),replyTo, sendCommand, askSenderNickName)
import Network.IRC.Bot.Parsec   (parsecPart)
import Text.Parsec              ((<|>), string, try)
import Text.Parsec.Combinator
import qualified Network.IRC.Commands as Commands
import Data.Maybe
import Debug.Trace
import Control.Monad (when)


{-
TODO: Maybe we should have a shallow embedding instead? If we do, we can keep
      all the computations inside the monad
TODO:
The best way to add new functionality to the bot seems to be to add new "Parts"
The rest of the library code seems to involve organization around these parts,
so the DSL should probably focus on producing parts code, which is then injected
into a SimpleBot.

Separate from the Parts code is the configuration code, so if we can fuse the
connection code + the parts code we should have a functional bot.

To fulfill the course reqs we also need to havea deep understanding of the library
, we could for instance try to implement some data types as GADTs, and we could
also put some detailed flowchart in the report. The main loop is quite simple, and
should be a good thing to plot as a chart.

It would be nice if we could remove the parenthesis and also the ==> below. Using >>=
instead of ==> would be a nice change. Not sure if we can remove (), perhaps by making
set, nickname, server and name right assoc?

The part section of the DSL (see example 2 below) should probably generate a part, and add this
part to some internal state passed along. When we issue the connect command, these parts should be
injected into the bot.

Sample programs:
Connecting: (WORKING)
    set (nickname "Jonte") ==>
    set (server "irc.freenode.org") ==>
    set (name "Jonte") ==>
    connect

Connecting + greeting: (NOT WORKING)
    set (nickname "Jonte") ==>
    set (server "irc.freenode.org") ==>
    set (name "Jonte") ==>
    -- Below statement will generate a Part
    when-received "hello" do 
        respond "Hey there!" 
    -- end part
    connect

Googling: (NOT WORKING)
when-received "Google %s" do                        -- the %s should be parsed as string
    results <- getGoogleResults 5 query             -- query should contain contents of %s, <- is questionable
    respondGoogle "Here are your results" results   -- respondGoogle means we can avoid ++
-}
newtype DSL a = D {runDSL :: State -> Program -> (a, State, Program)}
type Program = [Command]
data Command = Set ConfigField 
                | CreatePart PartInstr 
                | Connect

data ConfigField = NickName String 
                | Server String 
                | Name String 
                | AddChannel String
                | AuthenticatedUser String
data PartInstr = Respond String 
                | WhenRecv String Program 
                | Say String String 
                | Kick String Recipient 
                | Part String
                | ChannelMode String String (Maybe Int) (Maybe Recipient) (Maybe String)
data Recipient = Usr String | CurrentUser

data State = State{
    parts   :: [BotPartT IO ()],
    cfg     :: BotConf,
    chanList:: Set String,
    kvs     :: [(String, [(String, String)])]
}

instance Monad DSL where
    return x = D $ \st p -> (x, st, p)
    p >>= k  = D $ \st px -> do
        let (b, st2, p2) = runDSL p st px
        runDSL (k b) st2 p2

getKVS :: State -> String -> Maybe [(String, String)]
getKVS st key = lookup key (kvs st)

addCommand :: a -> t -> [a] -> ((), t, [a])
addCommand c st p = ((), st, p ++ [c])

-- DSL
set :: ConfigField -> DSL ()
set s = D $ addCommand (Set s)

connect :: DSL ()
connect = D $ addCommand Connect

nickname :: String -> ConfigField
nickname = NickName

server :: String -> ConfigField
server = Server

name :: String -> ConfigField
name = Name

join :: String -> DSL ()
join s = D $ addCommand (Set (AddChannel s))

authenticated :: String -> DSL ()
authenticated user = D $ addCommand (Set (AuthenticatedUser user))

respond :: String -> DSL ()
respond s = D $ addCommand (CreatePart (Respond s))

whenReceived :: String -> DSL() -> DSL ()
whenReceived s p = D $ \st p2 -> do
    let (_, _, p3) = runDSL p st []
    addCommand (CreatePart (WhenRecv s p3)) st p2

sayTo :: String -> String -> DSL()
sayTo to s = D $ addCommand (CreatePart (Say to s))

kickUser :: String -> Recipient -> DSL ()
kickUser from who = D $ addCommand (CreatePart (Kick from who))

partChannel :: String -> DSL ()
partChannel from = D $ addCommand (CreatePart (Part from))


giveOps :: String -> Recipient -> DSL ()
giveOps chan user = --doIfAuthed user $ 
    D $ addCommand (CreatePart (ChannelMode chan "+o" Nothing (Just user) Nothing))

takeOps :: String -> Recipient -> DSL ()
takeOps chan user = --doIfAuthed user $ 
    D $ addCommand (CreatePart (ChannelMode chan "-o" Nothing (Just user) Nothing))

authenticate :: State -> String -> Bool
authenticate st user = case getKVS st "authedUsers" of
                            Nothing  -> False
                            Just kvs -> case lookup user kvs of
                                Just "authenticated" -> True
                                _                    -> False



run :: DSL () -> IO ()
run px = do
    let ((), st, p) = runDSL px starterState []
    return ()
    run' st p
    where 
          starterState =  State{cfg = nullBotConf{logger = stdoutLogger Debug}, 
                                                  parts = [pingPart], 
                                                  chanList = empty,
                                                  kvs = [("authedUsers", [("jonte", "authenticated")])]}
          run' :: State -> Program -> IO ()
          -- Configuration commands
          run' st (Set (AuthenticatedUser user):cmds) = run' newState cmds
            where newState = st{kvs = (kvs st)} -- TODO: implement me
          run' st (Set (NickName n):cmds) = run' newState cmds
            where newState = st{cfg = (cfg st){nick = n}}
          run' st (Set (Server srv):cmds)    = run' newState cmds
            where newState = st{cfg = (cfg st){host = srv}}
          run' st (Set (Name n):cmds)    = run' newState cmds
            where newState = st{cfg = (cfg st){user = User{
                                                        username = n,
                                                        realname = n,
                                                        hostname = n,
                                                        servername = n}}}
          run' st (Set (AddChannel c):cmds) = run' newState cmds
            where newState = st{chanList = insert c (chanList st)}
          -- Parts creation

          -- Move to runPart if possible
          run' st (CreatePart (WhenRecv s p):cmds) = run' newState cmds
            where newState = st{parts = newPart : parts st}
                  newPart = parsecPart $ do 
                                    try $ lookAhead $ string s
                                    runPart st p
                                    return ()
                                <|> return ()

          run' st (cp@(CreatePart _):cmds) = run' newState cmds
            where newState = st{parts = newPart : parts st}
                  newPart = runPart st [cp]

          -- Connect
          run' st (Connect : cmds)           = do
            (_, chanPart) <- initChannelsPart (chanList st)
            simpleBot (cfg st) (parts st ++ [chanPart])
            run' st cmds
          -- Display entered data
          run' _ [] = return ()

runPart :: BotMonad m => State -> [Command] -> m ()
runPart _ [] = return ()
runPart st (CreatePart cmd:cmds) = 
    case cmd of
        (Say to s) -> sendCommand (PrivMsg Nothing [to] s)
        (Respond s) -> do 
                    target <- maybeZero =<< replyTo
                    sendCommand (PrivMsg Nothing [target] s)
        (Kick from CurrentUser)    -> doIfAuthed $
            askSenderNickName >>= \(Just u) ->
                sendMessage (Commands.kick from u Nothing)
        (Kick from (Usr user)) -> doIfAuthed $
            sendMessage (Commands.kick from user Nothing)
        (Part from ) -> sendMessage (Commands.part from)
        (ChannelMode c cm l u bm) -> doIfAuthed $
            case u of
                Just CurrentUser -> do
                    target <- askSenderNickName
                    sendMessage (Commands.channelMode c cm l target bm)
                Just (Usr target) ->
                    sendMessage (Commands.channelMode c cm l (Just target) bm)
                _ ->
                    sendMessage (Commands.channelMode c cm l Nothing bm)

    >> runPart st cmds

    where   
--        doIfAuthed :: String -> DSL () -> DSL ()
        doIfAuthed prog = do
            Just user <- askSenderNickName
            let isAuthed = authenticate st user
            when isAuthed prog
