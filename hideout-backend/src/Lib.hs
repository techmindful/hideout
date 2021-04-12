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

import           Chat
    
import qualified Servant
import           Servant ( (:>), (:<|>)(..), Capture, Get, NoContent(..), Put, ReqBody )
import           Servant.API.WebSocket ( WebSocket )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.Cors ( cors, CorsResourcePolicy(..) )
import qualified Network.WebSockets as WebSock

import qualified Data.Aeson as Aeson
import           Data.Aeson ( FromJSON, ToJSON )
import           Crypto.Random ( seedNew, seedToInteger )
import           Crypto.Hash ( SHA256(..), hashWith )

import           Control.Exception ( finally )
import           Control.Lens ( (^.), (.~), (%~) )
import           Control.Monad ( forever, forM_ )
import           Control.Monad.IO.Class
import           Control.Monad.Reader ( ReaderT, ask, runReaderT )
import qualified Control.Monad.STM as STM
import           Control.Monad.STM ( atomically )
import           Control.Concurrent.STM.TVar ( TVar, newTVar, readTVar, writeTVar )
import qualified Data.ByteString.Lazy as LazyByteStr
import qualified Data.ByteString as ByteStr
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


data AppState = AppState
  { letterMetas :: TVar ( Map String LetterMeta )
  , chats :: TVar ( Map ChatId Chat )
  } deriving ( Generic )


type API = "read-letter"  :> Capture "letterId" String :> Get '[ Servant.JSON ] LetterMeta
      :<|> "write-letter" :> ReqBody '[ Servant.JSON ] Letter :> Put '[ Servant.JSON ] String
      :<|> "new-chat"     :> Get '[ Servant.JSON ] String
      :<|> "chat"         :> Capture "chatId" String :> WebSocket


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

  newChatIdStr <- liftIO $ do
    
    oldChats <- atomically $ readTVar ( appState ^. #chats )

    newChatIdStr <- getRandomHash

    -- Create and insert new Chat into AppState.

    let newChatId = ChatId newChatIdStr
    
    let newChat = Chat {
          users = Map.empty
        , msgs = []
        , joinCount = 0
        , maxJoinCount = 2
        }

    let newChats = Map.insert newChatId newChat oldChats

    atomically $ writeTVar ( appState ^. #chats ) newChats

    return newChatIdStr

  return newChatIdStr


chatHandler :: String -> WebSock.Connection -> ReaderT AppState Servant.Handler ()
chatHandler chatIdStr conn = do

  appState <- ask

  let getChats = atomically $ readTVar ( appState ^. #chats )

  let thisChatId = ChatId chatIdStr

  chatsBeforeJoin <- liftIO getChats

  -- Before joining, first check if chat exists.
  let maybeChat = Map.lookup thisChatId chatsBeforeJoin
  case maybeChat of
    Nothing -> do
      liftIO $ putStrLn "Chat doesn't exist."  -- TODO: Report to client.
    -- Chat exists: Handle join, enter loop.
    Just chat -> liftIO $ do
      -- Check join count.
      if chat ^. #joinCount == chat ^. #maxJoinCount then
        putStrLn "Maximum join count is reached."  -- TODO: Report to client.
      else do
        let newUserId = chat ^. #joinCount

        let newUser = User {
              name = "User" ++ show newUserId
            , userConn = conn
            }

        let newChat  = chat { users = Map.insert newUserId newUser $ chat ^. #users
                            , joinCount = ( chat & joinCount ) + 1
                            }
            newChats = Map.insert thisChatId newChat chatsBeforeJoin
           
        let removeUser :: IO ()
            removeUser = liftIO $ do
              chats <- atomically $ readTVar $ appState ^. #chats
              let  maybeChat = Map.lookup thisChatId chats
              case maybeChat of
                -- Chat doesn't exist in AppState anymore, maybe removed by other threads,
                -- So no need to remove?
                Nothing -> return ()
                Just chat -> do
                  let newChat  = chat & #users %~ Map.delete newUserId
                      newChats = Map.insert thisChatId newChat chats
                  atomically $ writeTVar ( appState ^. #chats ) newChats

        let loop :: TVar ( Map ChatId Chat ) -> IO ()
            loop chatsTvar = liftIO $ do
              -- Check for incoming message.
              -- THIS BLOCKS THE THREAD!!!
              dataMsg <- WebSock.receiveDataMessage conn
              -- First check if chat *still* exists.
              chats <- atomically $ readTVar chatsTvar
              let maybeChat = Map.lookup thisChatId chats
              case maybeChat of
                Nothing -> do
                  putStrLn "Chat doesn't exist."  -- TODO: Report to client.
                -- Chat exists.
                Just chat -> do
                  case dataMsg of
                    WebSock.Text byteStr _ -> do
                      let maybeMsgFromClient :: Maybe MsgFromClient
                          maybeMsgFromClient = Aeson.decode byteStr
                      case maybeMsgFromClient of
                        Nothing -> putStrLn "Message can't be JSON-decoded."
                        Just msgFromClient -> do
                          let msgType = msgFromClient ^. #msgType
                              msgBody = msgFromClient ^. #msgBody
                          -- Debug print msg.
                          putStrLn $ "Received msg from client: " ++ show msgFromClient
                          -- Broadcast the msg.
                          let msgFromServer = MsgFromServer {
                              msgFromClient = msgFromClient
                            , username = newUser & name
                          }
                          forM_ ( Map.elems $ chat ^. #users ) $
                            ( \user ->
                                WebSock.sendTextData
                                  ( user & userConn )
                                  ( Aeson.encode msgFromServer )
                            )

                    _ -> liftIO $ putStrLn "Data Message is in binary form."

                  loop chatsTvar

        -- Update AppState.
        atomically $ writeTVar ( appState ^. #chats ) newChats

        -- Enter loop.
        WebSock.withPingThread conn 30 ( return () ) $
          flip finally removeUser $ loop $ appState ^. #chats


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
    :<|> chatHandler


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
  initChats       <- atomically $ newTVar Map.empty
  let initAppState = AppState {
      letterMetas  = initLetterMetas
    , chats        = initChats
    }

  Warp.run 8080 $ app initAppState
