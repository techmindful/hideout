{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}

module App
    ( startApp
    , app
    ) where

import           Letter
import           Chat
import           Utils ( readPosInt )
    
import qualified Servant
import           Servant ( (:>), (:<|>)(..), Capture, Get, NoContent(..), Put, ReqBody )
import           Servant.API.WebSocket ( WebSocket )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.Cors ( cors, CorsResourcePolicy(..) )
import qualified Network.WebSockets as WebSock

import           Crypto.Random ( seedNew, seedToInteger )
import           Crypto.Hash ( SHA256(..), hashWith )
import qualified Database.Persist as Persist
import           Database.Persist.Class ( selectList )
import           Database.Persist.Sql ( SqlBackend, runMigration, runSqlPool )
import           Database.Persist.Sqlite ( createSqlitePool )

import           Control.Error.Util ( failWith )
import           Control.Exception ( finally )
import           Control.Lens ( (^.), (.~), (%~) )
import           Control.Monad ( forever, forM_ )
import           Control.Monad.Except ( ExceptT, runExceptT )
import           Control.Monad.IO.Class
import           Control.Monad.Logger ( runStderrLoggingT )
import           Control.Monad.Reader ( ReaderT, ask, runReaderT )
import qualified Control.Monad.STM as STM
import           Control.Monad.STM ( atomically )
import           Control.Concurrent.STM.TVar ( TVar, newTVar, readTVar, writeTVar )
import qualified Data.Aeson as Aeson
import           Data.Aeson ( FromJSON, ToJSON )
import qualified Data.ByteString.Lazy as LazyByteStr
import qualified Data.ByteString as ByteStr
import qualified Data.ByteString.Char8 as ByteStrC8
import           Data.Function ( (&) )
import           Data.Generics.Labels
import qualified Data.Map.Strict as Map
import           Data.Map.Strict ( Map(..) )
import           Data.Pool ( Pool )
import           Data.Text ( Text )
import           GHC.Generics ( Generic )


data AppState = AppState
  { dbConnPool :: Pool SqlBackend
  , letterMetas :: TVar ( Map String LetterMeta )
  , chats :: TVar ( Map ChatId Chat )
  } deriving ( Generic )


type API = "read-letter"  :> Capture "letterId" String :> Get '[ Servant.JSON ] LetterMeta
      :<|> "write-letter" :> ReqBody '[ Servant.JSON ] Letter :> Put '[ Servant.JSON ] String
      :<|> "spawn-disposable-chat" :> ReqBody '[ Servant.PlainText ] String
                                   :> Put '[ Servant.JSON ] String
      :<|> "spawn-persistent-chat" :> ReqBody '[ Servant.PlainText ] String
                                   :> Put '[ Servant.JSON ] String
      :<|> "chat"         :> Capture "chatId" String :> WebSocket


readLetter :: String -> ReaderT AppState Servant.Handler LetterMeta
readLetter letterId = do

  appState <- ask

  oldLetterMetas <- liftIO $ atomically $ readTVar ( appState & letterMetas )

  let maybeLetterMeta = Map.lookup letterId oldLetterMetas
  case maybeLetterMeta of
    Nothing -> Servant.throwError Servant.err404
    Just oldLetterMeta -> do

      let newLetterMeta = oldLetterMeta & #letterMetaReadCount %~ ( (+1) :: Int -> Int )

          -- Delete letter from memory if maxReadCount is reached.
          -- Otherwise, increment the read count and update the Map.
          newLetterMetas =
            if newLetterMeta ^. #letterMetaReadCount ==
               newLetterMeta ^. #letterMetaLetter . #letterMaxReadCount then
              Map.delete letterId oldLetterMetas
            else
              Map.insert letterId newLetterMeta oldLetterMetas

      liftIO $ atomically $ writeTVar ( appState & letterMetas ) newLetterMetas

      return newLetterMeta



writeLetter :: Letter -> ReaderT AppState Servant.Handler String
writeLetter letter = mkNewLetter letter


mkNewLetter :: Letter -> ReaderT AppState Servant.Handler String
mkNewLetter letter = do

  appState <- ask

  letterId <- liftIO $ do

    oldLetterMetas <- atomically $ readTVar ( appState & letterMetas )

    hash <- getRandomHash

    -- Create a new LetterMeta, and insert it into AppState.
    let newLetterMeta  = LetterMeta { letterMetaLetter = letter, letterMetaReadCount = 0 }
        newLetterMetas = Map.insert hash newLetterMeta oldLetterMetas
    atomically $ writeTVar ( appState & letterMetas ) newLetterMetas

    -- TODO: This is just a test save.
    runSqlPool ( Persist.insert $ LetterMetaWithId hash newLetterMeta ) ( appState ^. #dbConnPool )

    return hash

  return letterId


spawnDispChat :: String -> ReaderT AppState Servant.Handler String
spawnDispChat maxJoinCountInput = do

  case readPosInt maxJoinCountInput of
    Just int -> do
      mkNewChat $
        Chat.Config {
          maxJoinCount = Just int 
        , expiry = MaxJoined
        , persist = False
        , sendHistory = True
        }

    Nothing -> Servant.throwError Servant.err400


spawnPersistChat :: String -> ReaderT AppState Servant.Handler String
spawnPersistChat maxJoinCountInput = do

  case readPosInt maxJoinCountInput of
    Just int -> do
      newChatIdStr <- mkNewChat $
        Chat.Config
          { maxJoinCount = Nothing
          , expiry = Never
          , persist = True
          , sendHistory = True
          }

      let chatIdLetter = Letter {
            letterBody =
              "You are invited to a Hideout persistent chat. Below is the link to the chat room. Bookmark the chat (not this letter), and you can send private messages to your contacts at any time.\nDo not post the chat link anywhere.\n[http://localhost:8000/chat/" ++ newChatIdStr ++ "](http://localhost:8000/chat/" ++ newChatIdStr ++ ")"

          , letterMaxReadCount = int
          }

      mkNewLetter chatIdLetter

    Nothing -> Servant.throwError Servant.err400


mkNewChat :: Chat.Config -> ReaderT AppState Servant.Handler String
mkNewChat config = do

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
        , config = config
        }

    let newChats = Map.insert newChatId newChat oldChats

    atomically $ writeTVar ( appState ^. #chats ) newChats

    return newChatIdStr

  return newChatIdStr


isMaxJoined :: Chat -> Bool
isMaxJoined chat =
  case chat ^. #config . #maxJoinCount of
    Just posInt ->
      if chat ^. #joinCount == posInt then True
      else False

    _ -> False


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
    -- Chat exists:
    Just chat -> liftIO $ do
      if isMaxJoined chat then
        putStrLn "Maximum join count is reached."  -- TODO: Report to client.
      -- Can join:
      else do
        let userId = chat ^. #joinCount

        let user = User {
              name = "User" ++ show userId
            , conn = conn
            }

        let newChat  = chat { users = Map.insert userId user $ chat ^. #users
                            , joinCount = ( chat & joinCount ) + 1
                            }
            newChats = Map.insert thisChatId newChat chatsBeforeJoin

        -- | This blocks thread too!
        -- If a user already disconnected, this blocks broadcasting to all other users!
        let broadcast :: MonadIO m => Chat -> LazyByteStr.ByteString -> m ()
            broadcast chat byteStr =
              liftIO $ forM_ ( Map.elems $ chat ^. #users ) $
                ( \user ->
                    WebSock.sendTextData ( user ^. #conn ) byteStr
                )
           
        let removeUser :: IO ()
            removeUser = liftIO $ do
              chats <- atomically $ readTVar $ appState ^. #chats
              let  maybeChat = Map.lookup thisChatId chats
              case maybeChat of
                -- Chat doesn't exist in AppState anymore, maybe removed by other threads,
                -- So no need to remove?
                Nothing -> return ()
                Just chat -> do
                  let username = case Map.lookup userId $ chat ^. #users of
                        Just user -> user ^. #name
                        Nothing -> "[Error: User not found]"
                  let msgFromClient = MsgFromClient {
                        msgType = "leave"
                      , msgBody = ""
                      }
                  let msgFromServer = MsgFromServer {
                        msgFromClient = msgFromClient
                      , username = username
                      }
                  let newChat  = chat & #users %~ Map.delete userId
                                      & #msgs  %~ ( ++ [ msgFromServer ] )
                      newChats = Map.insert thisChatId newChat chats
                  -- Update AppState.
                  atomically $ writeTVar ( appState ^. #chats ) newChats
                  -- Important to broadcast to new chat,
                  -- Don't broadcast to disconnected users. That blocks the whole thing.
                  broadcast newChat $ Aeson.encode msgFromServer

        let loop :: TVar ( Map ChatId Chat ) -> ExceptT String IO ()
            loop chatsTvar = do
              -- Check for incoming message.
              -- THIS BLOCKS THE THREAD!!!
              -- ABSOLUTELY HAVE TO DO THIS BEFORE READING APPSTATE.
              dataMsg <- liftIO $ WebSock.receiveDataMessage conn

              chats <- liftIO $ atomically $ readTVar chatsTvar

              chat  <- failWith "Chat doesn't exist."   $ Map.lookup thisChatId chats
              user  <- failWith "User doesn't exist..?" $ Map.lookup userId $ chat ^. #users

              case dataMsg of
                WebSock.Text byteStr _ -> do

                  msgFromClient <- failWith "Message can't be JSON-decoded." $ Aeson.decode byteStr

                  -- Debug print msg.
                  liftIO $ putStrLn $ "Received msg from client: " ++ show msgFromClient

                  let msgType = msgFromClient ^. #msgType
                      msgBody = msgFromClient ^. #msgBody

                  let msgFromServer = MsgFromServer {
                        msgFromClient = msgFromClient
                      , username = user ^. #name
                      }

                  -- Update chat.
                  let
                    -- Update msgs.
                    chat'  = chat & #msgs %~ ( ++ [ msgFromServer ] )

                    -- Update based on msg type.
                    chat'' =
                      case msgType of
                        "nameChange" ->
                          let updatedUser = user & #name .~ msgBody
                          in chat' & #users %~ Map.insert userId updatedUser

                        _ -> chat'

                  -- Update chats.
                  let updatedChats = Map.insert thisChatId chat'' chats
                  liftIO $ atomically $ writeTVar chatsTvar updatedChats
 
                  -- Broadcast the msg.
                  broadcast chat'' $ Aeson.encode msgFromServer

                _ -> liftIO $ putStrLn "Data Message is in binary form."

              liftIO $ loopAndHandleError $ loop chatsTvar


            loopAndHandleError :: ExceptT String IO () -> IO ()
            loopAndHandleError loop =
              runExceptT loop >>= \case
                Left errStr -> putStrLn errStr
                Right _     -> return ()


        -- Update AppState.
        atomically $ writeTVar ( appState ^. #chats ) newChats

        -- Give msg history first.
        let msgHistory = MsgHistory {
          msgs  = chat ^. #msgs
        , users = fmap ( ^. #name ) $ Map.elems $ chat ^. #users
        }
        WebSock.sendTextData ( user ^. #conn ) ( Aeson.encode msgHistory )

        -- Enter loop.
        WebSock.withPingThread conn 30 ( return () ) $
          flip finally removeUser $ loopAndHandleError $ loop $ appState ^. #chats


getRandomHash :: IO String
getRandomHash = do
  seed <- seedNew
  let seedStr = show $ seedToInteger seed
      hash    = show $ hashWith SHA256 $ ByteStrC8.pack seedStr
  return hash


server :: Servant.ServerT API ( ReaderT AppState Servant.Handler )
server = readLetter
    :<|> writeLetter
    :<|> spawnDispChat
    :<|> spawnPersistChat
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

  dbConnPool <- runStderrLoggingT $ createSqlitePool "database.db" 5
  runSqlPool ( runMigration migrateAll ) dbConnPool

  dbLetterMetaWithIds :: [ Persist.Entity LetterMetaWithId ] <-
    runSqlPool ( selectList [] [] ) dbConnPool

  let letterMetaWithIds = fmap Persist.entityVal dbLetterMetaWithIds

      split :: LetterMetaWithId -> ( String, LetterMeta )
      split letterMetaWithId =
        ( letterMetaWithId & letterMetaWithIdId'
        , letterMetaWithId & letterMetaWithIdVal
        )

      idLetterMetaPairs = fmap split letterMetaWithIds

  initLetterMetas <- atomically $ newTVar $ Map.fromList idLetterMetaPairs
  initChats       <- atomically $ newTVar Map.empty
  let initAppState = AppState {
      dbConnPool   = dbConnPool
    , letterMetas  = initLetterMetas
    , chats        = initChats
    }

  Warp.run 8080 $ app initAppState
