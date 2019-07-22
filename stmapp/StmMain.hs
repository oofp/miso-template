module Main where

import Miso
import Miso.String
import Control.Monad.IO.Class
import Language.Javascript.JSaddle.Warp as JSaddle
import Data.JSString.Text
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import StmContainers.Map as STMMap
import Data.Text

data Model = Model 
    { buffer :: Text
    , sessionID :: Maybe Int
    } deriving Eq

defaultModel :: Model
defaultModel = Model "" Nothing

data Event
    = NoEvent
    | Inc
    | Dec
    | SayHello
    | MessageReceived Text
    | AppStarted 
    | SessionStarted Int
    | MsgSent
    | CommBroken
    deriving (Eq, Show)

readNextMessage :: Int -> SessionMap -> JSM Event
readNextMessage sessID sessionMap = liftIO $ do
    liftIO $ putStrLn "Wait for next message..."
    channelsMaybe <- atomically $ STMMap.lookup sessID sessionMap
    case channelsMaybe of  
        Just (_, recvChan) -> fmap MessageReceived (atomically $ readTChan recvChan)
        Nothing -> return CommBroken  

type SessionMap = Map Int (TChan Text, TChan Text)

sendMessage :: SessionMap -> Model -> Text -> JSM Event
sendMessage sessionMap (Model buffer (Just sessID)) cmd = liftIO $ do
    channelsMaybe <- atomically $ STMMap.lookup sessID sessionMap
    let msg = cmd <> " " <> buffer
    putStrLn $ Data.Text.unpack $ "Sending message:" <> msg
    case channelsMaybe of  
        Just (sendChan, _) -> atomically $ writeTChan sendChan msg >> return MsgSent
        Nothing -> return CommBroken  
sendMessage _ _ _ = return CommBroken

update' :: TVar Int -> SessionMap -> Event -> Model -> Effect Event Model
update' sessionCounter sessionMap event model = case event of
    NoEvent -> noEff model
    Inc     -> model <# sendMessage sessionMap model "append"
    Dec     -> model <# sendMessage sessionMap model "truncate"
    SayHello -> model <# do
        liftIO $ putStrLn "Hello!"
        pure NoEvent
    MessageReceived msg->  
        case sessionID model of 
            Just sessID -> model {buffer = msg} <# readNextMessage sessID sessionMap
            Nothing -> noEff model 
    AppStarted -> model <# startSession sessionCounter sessionMap
    SessionStarted sessID -> model {sessionID=Just sessID, buffer="***"} <# readNextMessage sessID sessionMap
    MsgSent -> model <# liftIO (putStrLn "MsgSent" >> return NoEvent)
    CommBroken -> model <# liftIO (putStrLn "CommBroken!" >> return NoEvent)

startSession :: TVar Int -> SessionMap -> JSM Event     
startSession sessionCounter sessionMap = liftIO $ do 
        (newCnt, (sendChan,recvChan)) <- atomically $ do
            curCounter <- readTVar sessionCounter
            let newCounter = curCounter + 1 
            writeTVar sessionCounter newCounter
            sendChan <- newTChan
            recvChan <- newTChan
            insert (sendChan,recvChan) newCounter sessionMap
            return (newCounter,(sendChan,recvChan))
        async $ forever $ do
            cmd_buf <- atomically $ readTChan sendChan
            case Data.Text.words cmd_buf of 
                ["append",buf] -> atomically $ writeTChan recvChan ("*" <> buf) 
                ["truncate"] -> atomically $ writeTChan recvChan ("0") 
                ["truncate",buf] -> atomically $ writeTChan recvChan (Data.Text.tail buf) 
                _ -> putStrLn ("Cannot detect command:" <> Data.Text.unpack cmd_buf)
        return $ SessionStarted newCnt


view' :: Model -> View Event
view' model = div_ []
    [ text "Hello ~ Haskell GUI"
    , br_ []
    , button_ [ onClick Inc ] [ text "+" ]
    , text $ textToJSString $ buffer model
    , button_ [ onClick Dec ] [ text "-" ]
    , br_ []
    , button_ [ onClick SayHello ] [ text "Say Hello!" ]
    ]

main :: IO ()
main = do
    sessionMap <- newIO
    sessionCounter <- newTVarIO 0 
    let update = update' sessionCounter sessionMap 
    JSaddle.run 8000 $ startApp App {..}
  where
    initialAction = AppStarted
    model  = defaultModel
    view   = view'
    events = defaultEvents
    subs   = []
    mountPoint = Nothing