{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}

module Lib
    ( startApp
    , app
    ) where
    
import qualified Servant
import           Servant ( (:>), (:<|>)(..), Capture, Get, NoContent(..), Put, ReqBody )
import           Servant.API.WebSocket ( WebSocket )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.Cors ( cors, CorsResourcePolicy(..) )
import qualified Network.WebSockets as WebSock

import           Data.Aeson ( FromJSON, ToJSON )
import           Crypto.Random ( seedNew, seedToInteger )
import           Crypto.Hash ( SHA256(..), hashWith )

import           Control.Lens ( (^.), (.~), (%~) )
import           Control.Monad ( forever )
import           Control.Monad.IO.Class
import           Control.Monad.Reader ( ReaderT, ask, runReaderT )
import           Control.Monad.STM ( atomically )
import           Control.Concurrent.STM.TVar ( TVar, newTVar, readTVar, writeTVar )
import qualified Data.ByteString.Char8 as ByteStrC8
import           Data.Function ( (&) )
import           Data.Generics.Labels
import qualified Data.Map.Strict as Map
import           Data.Map.Strict ( Map(..) )
import           GHC.Generics ( Generic )


data Letter = Letter
  { body :: String
  , maxReadCount :: Int
  }
  deriving ( Generic, Show )
instance FromJSON Letter
instance ToJSON Letter


data LetterMeta = LetterMeta
  { letter  :: Letter
  , readCount :: Int
  }
  deriving ( Generic, Show )
instance FromJSON LetterMeta
instance ToJSON LetterMeta


data Message = Message
  { body :: String }
  deriving ( Generic, Show )
instance FromJSON Message
instance ToJSON Message


data Chat = Chat
  { messages :: [ Message ]
  , maxJoinCount :: Int
  }
  deriving ( Generic, Show )
instance FromJSON Chat
instance ToJSON Chat


data ChatMeta = ChatMeta
  { chat :: Chat
  , joinCount :: Int
  }
  deriving ( Generic, Show )
instance FromJSON ChatMeta
instance ToJSON ChatMeta


type API = "read-letter"  :> Capture "letterId" String :> Get '[ Servant.JSON ] LetterMeta
      :<|> "write-letter" :> ReqBody '[ Servant.JSON ] Letter :> Put '[ Servant.JSON ] String
      :<|> "new-chat"     :> Get '[ Servant.JSON ] String
      :<|> "send-message" :> Capture "chatId" String :> WebSocket


data AppState = AppState
  { letterMetas :: TVar ( Map String LetterMeta )
  , chatMetas :: TVar ( Map String ChatMeta )
  }


readLetter :: String -> ReaderT AppState Servant.Handler LetterMeta
readLetter letterId = do

  appState <- ask

  oldLetterMetas <- liftIO $ atomically $ readTVar ( appState & letterMetas )

  let maybeLetterMeta = Map.lookup letterId oldLetterMetas
  case maybeLetterMeta of
    Nothing -> Servant.throwError Servant.err404
    Just oldLetterMeta -> do

      let newLetterMeta = oldLetterMeta & #readCount %~ ( (+1) :: Int -> Int )

          -- Delete letter from memory if maxReadCount is reached.
          -- Otherwise, increment the read count and update the Map.
          newLetterMetas =
            if newLetterMeta ^. #readCount == newLetterMeta ^. #letter . #maxReadCount then
              Map.delete letterId oldLetterMetas
            else
              Map.insert letterId newLetterMeta oldLetterMetas

      liftIO $ atomically $ writeTVar ( appState & letterMetas ) newLetterMetas

      return newLetterMeta



writeLetter :: Letter -> ReaderT AppState Servant.Handler String
writeLetter letter = do

  appState <- ask

  letterId <- liftIO $ do

    oldLetterMetas <- atomically $ readTVar ( appState & letterMetas )

    hash <- getRandomHash

    -- Create a new LetterMeta, and insert it into AppState.
    let newLetterMeta  = LetterMeta { letter = letter, readCount = 0 }
        newLetterMetas = Map.insert hash newLetterMeta oldLetterMetas
    atomically $ writeTVar ( appState & letterMetas ) newLetterMetas

    -- Debug print
    letterMetas <- atomically $ readTVar ( appState & letterMetas )
    putStrLn $ "Current letter metas:\n----------\n" ++  show letterMetas

    return hash

  return letterId


newChat :: ReaderT AppState Servant.Handler String
newChat = do

  appState <- ask

  chatId <- liftIO $ do
    
    oldChatMetas <- atomically $ readTVar ( appState & chatMetas )

    chatId <- getRandomHash

    -- Create and insert new ChatMeta into AppState.
    let newChat = Chat { messages = [], maxJoinCount = 2 }
        newChatMeta = ChatMeta { chat = newChat, joinCount = 0 }
        newChatMetas = Map.insert chatId newChatMeta oldChatMetas
    atomically $ writeTVar ( appState & chatMetas ) newChatMetas

    return chatId

  return chatId


sendMessage :: String -> WebSock.Connection -> ReaderT AppState Servant.Handler ()
sendMessage chatId conn = forever $ do

  appState <- ask

  message <- liftIO $ WebSock.receiveDataMessage conn
  
  liftIO $ putStrLn $ show message

  --oldChatMetas <- liftIO $ atomically $ readTVar ( appState & chatMetas )
  --let maybeOldChatMeta = Map.lookup chatId oldChatMetas
  --case maybeOldChatMeta of
  --  Nothing -> do
  --    liftIO $ print "404"
  --    Servant.throwError Servant.err404
  --  Just oldChatMeta -> do
  --    let newChatMeta = oldChatMeta & #chat . #messages %~ ( ( : ) message )
  --        newChatMetas = Map.insert chatId newChatMeta oldChatMetas
  --    liftIO $ atomically $ writeTVar ( appState & chatMetas ) newChatMetas

  --    liftIO $ putStrLn $ show newChatMetas

  --return ()


getRandomHash :: IO String
getRandomHash = do
  seed <- seedNew
  let seedStr = show $ seedToInteger seed
      hash    = show $ hashWith SHA256 $ ByteStrC8.pack seedStr
  return hash


server :: Servant.ServerT API ( ReaderT AppState Servant.Handler )
server = readLetter
    :<|> writeLetter
    :<|> newChat
    :<|> sendMessage


api :: Servant.Proxy API
api = Servant.Proxy


app :: AppState -> Servant.Application
app appState =
  cors ( \_ -> Just $ CorsResourcePolicy
          { corsOrigins = Nothing
          , corsMethods = [ "GET", "PUT" ]
          , corsRequestHeaders = [ "Content-Type" ]
          , corsExposedHeaders = Nothing
          , corsMaxAge = Nothing  -- TODO: Change in production
          , corsVaryOrigin = True
          , corsRequireOrigin = False
          , corsIgnoreFailures = False
          }
       )
  ( Servant.serve api $ Servant.hoistServer api ( \x -> runReaderT x appState ) server )



startApp :: IO ()
startApp = do
  initLetterMetas <- atomically $ newTVar Map.empty
  initChatMetas   <- atomically $ newTVar Map.empty
  let initAppState = AppState {
      letterMetas = initLetterMetas
    , chatMetas   = initChatMetas
    }

  Warp.run 8080 $ app initAppState
