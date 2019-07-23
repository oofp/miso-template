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
    , sessionID :: Int
    } deriving Eq

data Event
    = NoEvent
    | Inc
    | Dec
    | SayHello
    | MessageReceived Text
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
sendMessage sessionMap (Model buffer sessID) cmd = liftIO $ do
    channelsMaybe <- atomically $ STMMap.lookup sessID sessionMap
    let msg = cmd <> " " <> buffer
    putStrLn $ Data.Text.unpack $ "Sending message:" <> msg
    case channelsMaybe of  
        Just (sendChan, _) -> atomically $ writeTChan sendChan msg >> return MsgSent
        Nothing -> return CommBroken  

update' :: TVar Int -> SessionMap -> Event -> Model -> Effect Event Model
update' sessionCounter sessionMap event model = case event of
    NoEvent -> noEff model
    Inc     -> model <# sendMessage sessionMap model "append"
    Dec     -> model <# sendMessage sessionMap model "truncate"
    SayHello -> model <# do
        liftIO $ putStrLn "Hello!"
        pure NoEvent
    MessageReceived msg-> noEff model {buffer = msg}
    MsgSent -> model <# liftIO (putStrLn "MsgSent" >> return NoEvent)
    CommBroken -> model <# liftIO (putStrLn "CommBroken!" >> return NoEvent)

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


initComm :: TVar Int -> SessionMap -> IO (Int,  (TChan Text,TChan Text))     
initComm sessionCounter sessionMap = atomically $ do
    curCounter <- readTVar sessionCounter
    let newCounter = curCounter + 1 
    writeTVar sessionCounter newCounter
    sendChan <- newTChan
    recvChan <- newTChan
    insert (sendChan,recvChan) newCounter sessionMap
    return (newCounter,(sendChan,recvChan)) 
        
serverHandling :: TChan Text -> TChan Text -> IO () 
serverHandling sendChan recvChan = do
    void $ async $ forever $ do
        cmd_buf <- atomically $ readTChan sendChan
        case Data.Text.words cmd_buf of 
            ["append"] -> atomically $ writeTChan recvChan "1" 
            ["append",buf] -> atomically $ writeTChan recvChan ("*" <> buf) 
            ["truncate"] -> atomically $ writeTChan recvChan "0" 
            ["truncate",buf] -> atomically $ writeTChan recvChan (Data.Text.tail buf) 
            _ -> putStrLn ("Cannot detect command:" <> Data.Text.unpack cmd_buf)

initRecvSub :: TChan Text -> Sub Event 
initRecvSub recvChan sink = liftIO $ void . async . forever $ do
    fmap MessageReceived (atomically $ readTChan recvChan) >>= sink
            
initApp :: TVar Int -> SessionMap -> JSM (Int, Sub Event)
initApp sessionCounter sessionMap = do
    (newCnt, (sendChan,recvChan)) <- liftIO $ initComm sessionCounter sessionMap
    liftIO $ serverHandling sendChan recvChan
    let sub = initRecvSub recvChan  
    return (newCnt,sub)

main :: IO ()
main = do
    sessionMap <- newIO
    sessionCounter <- newTVarIO 0 
    let update = update' sessionCounter sessionMap 
    JSaddle.run 8000 $ do 
        (sessID, recvSub) <- initApp sessionCounter sessionMap
        let initialAction = NoEvent
            model  = Model "" sessID
            view   = view'
            events = defaultEvents
            subs   = [recvSub]
            mountPoint = Nothing        
        startApp App {..}
