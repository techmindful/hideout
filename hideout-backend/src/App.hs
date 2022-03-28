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
  ( API
  , api
  , startApp
  , initApp
  , mkApp
  ) where


import           DbTypes
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
import           Database.Persist ( (=.), (==.), (+=.) )
import           Database.Persist.Class ( selectList )
import           Database.Persist.Sql ( SqlBackend, runMigration, runSqlPool )
import           Database.Persist.Sqlite ( createSqlitePool )

import           Control.Error.Util ( failWith )
import           Control.Exception ( finally )
import           Control.Lens ( (^.), (.~), (%~) )
import           Control.Monad ( forever, forM, forM_ )
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
import qualified Data.Text as Text
import           Data.Text ( Text, lines )
import qualified Data.Text.IO as TextIO
import           Data.Text.IO ( readFile )
import           Data.Time.Clock.POSIX ( getPOSIXTime )
import           GHC.Generics ( Generic )
import           Prelude hiding ( readFile, lines )


data AppState = AppState
  { dbConnPool :: Pool SqlBackend
  , letterMetas :: TVar ( Map LetterId LetterMeta )
  , entrances :: TVar ( Map Text Entrance )
  , chats :: TVar ( Map ChatId ( Chat, Map Int User ) )
  , wordlist :: [ Text ]
  } deriving ( Generic )


type API = "api" :> "read-letter"  :> Capture "letterId" LetterId :> Get '[ Servant.JSON ] LetterMeta
      :<|> "api" :> "write-letter" :> ReqBody '[ Servant.JSON ] Letter :> Put '[ Servant.JSON ] Text
      :<|> "api" :> "spawn-disposable-chat" :> ReqBody '[ Servant.PlainText ] Text
                                            :> Put '[ Servant.JSON ] Text
      :<|> "api" :> "spawn-persistent-chat" :> ReqBody '[ Servant.PlainText ] Text
                                            :> Put '[ Servant.JSON ] Text
      :<|> "api" :> "persist-chat-entrance" :> Capture "entranceId" Text :> Get '[ Servant.JSON ] Text
      :<|> "api" :> "chat" :> Capture "chatId" Text :> WebSocket


readLetter :: LetterId -> ReaderT AppState Servant.Handler LetterMeta
readLetter letterId = do

  appState <- ask

  oldLetterMetas <- liftIO $ atomically $ readTVar ( appState & letterMetas )

  let maybeLetterMeta = Map.lookup letterId oldLetterMetas
  case maybeLetterMeta of
    Nothing -> Servant.throwError Servant.err404
    Just oldLetterMeta -> liftIO $ do

      let newLetterMeta = oldLetterMeta & #readCount %~ ( (+1) :: Int -> Int )

      -- Max read count reached:
      if newLetterMeta ^. #readCount ==
         newLetterMeta ^. #letter . #maxReadCount then do

         -- Delete from memory.
         let newLetterMetas = Map.delete letterId oldLetterMetas
         atomically $ writeTVar ( appState & letterMetas ) newLetterMetas

         -- Delete from db.
         runSqlPool ( Persist.deleteBy $ UniqueLetterId letterId ) ( appState ^. #dbConnPool )

      else do

        -- Update AppState in memory.
        let newLetterMetas = Map.insert letterId newLetterMeta oldLetterMetas
        atomically $ writeTVar ( appState & letterMetas ) newLetterMetas

        -- Update db.
        runSqlPool
          ( Persist.updateWhere [ DbLetterMetaLetterId ==. letterId ] [ DbLetterMetaVal =. newLetterMeta ] )
          ( appState ^. #dbConnPool )

      return newLetterMeta


writeLetter :: Letter -> ReaderT AppState Servant.Handler Text
writeLetter letter = do

  appState <- ask

  liftIO $ do

    oldLetterMetas <- atomically $ readTVar ( appState & letterMetas )

    letterId <- fmap LetterId $ mkRandomId ( appState ^. #wordlist )

    -- Create a new LetterMeta, and insert it into AppState.
    let newLetterMeta  = LetterMeta { letter = letter, readCount = 0 }
        newLetterMetas = Map.insert letterId newLetterMeta oldLetterMetas
    atomically $ writeTVar ( appState & letterMetas ) newLetterMetas

    if letter ^. #persist then
      runSqlPool ( Persist.insert_ $ DbLetterMeta letterId newLetterMeta ) ( appState ^. #dbConnPool )
    else
      return ()

    pure $ unLetterId letterId


spawnDispChat :: Text -> ReaderT AppState Servant.Handler Text
spawnDispChat maxJoinCountInput = do

  case readPosInt maxJoinCountInput of
    Just int -> do
      chatId <- mkNewChat $
        Chat.Config {
          maxJoinCount = Just int 
        , expiry = MaxJoined
        , persist = False
        , sendHistory = True
        }
      pure $ unChatId chatId

    Nothing -> Servant.throwError Servant.err400


spawnPersistChat :: Text -> ReaderT AppState Servant.Handler Text
spawnPersistChat maxJoinCountInput = do

  appState <- ask

  case readPosInt maxJoinCountInput of
    Nothing -> Servant.throwError Servant.err400
    Just maxJoinCount -> do
      newChatId <- mkNewChat $
        Chat.Config
          { maxJoinCount = Nothing
          , expiry = Never
          , persist = True
          , sendHistory = True
          }

      newEntranceId <- liftIO $ mkRandomId ( appState ^. #wordlist )

      -- Save new entrance.
      liftIO $ do

        oldEntrances <- atomically $ readTVar $ appState ^. #entrances

        let newEntrance = Entrance {
              chatId = newChatId
            , maxViewCount = maxJoinCount
            , viewCount = 0
            }
            
            newEntrances = Map.insert newEntranceId newEntrance oldEntrances

        atomically $ writeTVar ( appState ^. #entrances ) newEntrances

        runSqlPool
          ( Persist.insert_ $ DbEntrance newEntranceId newEntrance )
          ( appState ^. #dbConnPool )

        return newEntranceId


mkNewChat :: Chat.Config -> ReaderT AppState Servant.Handler ChatId
mkNewChat config = do

  appState <- ask

  liftIO $ do

    oldChats <- atomically $ readTVar ( appState ^. #chats )

    randomId <- mkRandomId ( appState ^. #wordlist )

    -- Create and insert new Chat into AppState.

    let newChatId = ChatId randomId
    
    let newChat = Chat {
          msgs = []
        , joinCount = 0
        , config = config
        }

    -- Update memory AppState.
    let newChats = Map.insert newChatId ( newChat, Map.empty ) oldChats
    atomically $ writeTVar ( appState ^. #chats ) newChats

    -- Save to db, if persistent.
    if config ^. #persist then
      runSqlPool ( Persist.insert_ $ DbChat newChatId newChat ) ( appState ^. #dbConnPool )
    else
      return ()

    pure newChatId


isMaxJoined :: Chat -> Bool
isMaxJoined chat =
  case chat ^. #config . #maxJoinCount of
    Just posInt ->
      if chat ^. #joinCount == posInt then True
      else False

    _ -> False


persistChatEntrance :: Text -> ReaderT AppState Servant.Handler Text
persistChatEntrance entranceId = do

  appState <- ask

  oldEntrances <- liftIO $ atomically $ readTVar $ appState ^. #entrances

  case Map.lookup entranceId oldEntrances of
    Nothing -> Servant.throwError Servant.err404
    Just entrance -> liftIO $ do
      -- View count isn't reached.
      if ( entrance ^. #viewCount ) + 1 < entrance ^. #maxViewCount then do
        let newEntrance  = entrance & #viewCount %~ ( (+1) :: Int -> Int )
            newEntrances = Map.insert entranceId newEntrance oldEntrances

        atomically $ writeTVar ( appState ^. #entrances ) newEntrances

        runSqlPool
          ( Persist.updateWhere
            [ DbEntranceEntranceId ==. entranceId ]
            [ DbEntranceVal =. newEntrance ]
          )
          ( appState ^. #dbConnPool )
      -- View count is reached.
      else do
        let newEntrances = Map.delete entranceId oldEntrances
        atomically $ writeTVar ( appState ^. #entrances ) newEntrances
        runSqlPool ( Persist.deleteBy $ UniqueEntranceId entranceId ) ( appState ^. #dbConnPool )
        
      pure $ unChatId $ entrance ^. #chatId


chatHandler :: Text -> WebSock.Connection -> ReaderT AppState Servant.Handler ()
chatHandler chatIdText conn = do

  appState <- ask

  let getChats = atomically $ readTVar ( appState ^. #chats )

  let chatId = ChatId chatIdText

  chatsBeforeJoin <- liftIO getChats

  -- Before joining, first check if chat exists.
  let maybeChat = Map.lookup chatId chatsBeforeJoin
  case maybeChat of
    Nothing ->
      liftIO $ WebSock.sendTextData conn $ Aeson.encode Chat.notFoundCtrlMsg
    -- Chat exists:
    Just ( chat, users ) -> liftIO $ do
      if isMaxJoined chat then
        WebSock.sendTextData conn $ Aeson.encode Chat.maxJoinedCtrlMsg
      -- Can join:
      else do
        let userId = chat ^. #joinCount

        let user = User {
              name = "User" ++ show userId
            , conn = conn
            }

        let newChat  = chat { joinCount = ( chat ^. #joinCount ) + 1 }
            newUsers = Map.insert userId user users
            newChats = Map.insert chatId ( newChat, newUsers ) chatsBeforeJoin

        -- | This blocks thread too!
        -- If a user already disconnected, this blocks broadcasting to all other users!
        let broadcast :: MonadIO m => [ User ] -> LazyByteStr.ByteString -> m ()
            broadcast users byteStr =
              liftIO $ forM_ users $
                ( \user ->
                    WebSock.sendTextData ( user ^. #conn ) byteStr
                )
           
        let removeUser :: IO ()
            removeUser = liftIO $ do
              chats <- atomically $ readTVar $ appState ^. #chats
              let  maybeChat = Map.lookup chatId chats
              case maybeChat of
                -- Chat doesn't exist in AppState anymore, maybe removed by other threads,
                -- So no need to remove?
                Nothing -> return ()
                Just ( chat, users ) -> do

                  posixTime <- getPOSIXTime

                  let username = case Map.lookup userId users of
                        Just user -> user ^. #name
                        Nothing -> "[Error: User not found]"
                  let msgFromClient = MsgFromClient {
                        msgType = "leave"
                      , msgBody = ""
                      }
                  let chatMsgMeta = ChatMsgMeta {
                        msgFromClient = msgFromClient
                      , userId = userId
                      , username = username
                      , posixTimeSec = round posixTime
                      }
                  let newChat  = chat & #msgs  %~ ( ++ [ chatMsgMeta ] )
                      newUsers = Map.delete userId users
                      newChats = Map.insert chatId ( newChat, newUsers ) chats

                  -- Update AppState.
                  atomically $ writeTVar ( appState ^. #chats ) newChats
                  -- Important to broadcast to new chat,
                  -- Don't broadcast to disconnected users. That blocks the whole thing.
                  broadcast ( Map.elems newUsers ) $ Aeson.encode chatMsgMeta

        let loop :: TVar ( Map ChatId ( Chat, Map Int User ) ) -> ExceptT Text IO ()
            loop chatsTvar = do
              -- Check for incoming message.
              -- THIS BLOCKS THE THREAD!!!
              -- ABSOLUTELY HAVE TO DO THIS BEFORE READING APPSTATE.
              dataMsg <- liftIO $ WebSock.receiveDataMessage conn

              chats <- liftIO $ atomically $ readTVar chatsTvar

              ( chat, users ) <- failWith "Chat doesn't exist." $ Map.lookup chatId chats
              user <- failWith "User doesn't exist..?" $ Map.lookup userId $ users

              case dataMsg of
                WebSock.Text byteStr _ -> do

                  msgFromClient <- failWith "Message can't be JSON-decoded." $ Aeson.decode byteStr

                  posixTime <- liftIO getPOSIXTime

                  liftIO $ putStrLn $ "Time ( POSIX seconds ): " ++ ( show $ round posixTime )

                  -- Debug print msg.
                  liftIO $ putStrLn $ "Received msg from client: " ++ show msgFromClient

                  let msgType = msgFromClient ^. #msgType
                      msgBody = msgFromClient ^. #msgBody

                  let chatMsgMeta = ChatMsgMeta {
                        msgFromClient = msgFromClient
                      , userId = userId
                      , username = user ^. #name
                      , posixTimeSec = round posixTime
                      }

                  -- Update chat.
                  let
                    -- Update chat msgs, unless it's TypeHint.
                    chat'  = case msgType of
                      "typeHint" -> chat
                      _ -> chat & #msgs %~ ( ++ [ chatMsgMeta ] )

                    -- Update users based on msg type.
                    users' =
                      case msgType of
                        "nameChange" ->
                          let updatedUser = user & #name .~ msgBody
                          in Map.insert userId updatedUser users

                        _ -> users

                  -- Update chats in memory.
                  let updatedChats = Map.insert chatId ( chat', users' ) chats
                  liftIO $ atomically $ writeTVar chatsTvar updatedChats

                  -- Update chats in db, if persistent.
                  if chat ^. #config . #persist then liftIO $ do
                    runSqlPool
                      ( Persist.updateWhere 
                          [ DbChatChatId ==. chatId ]
                          [ DbChatVal =. chat' ]
                      )
                      ( appState ^. #dbConnPool )
                  else
                    return ()
 
                  -- Broadcast the msg.
                  broadcast ( Map.elems users' ) $ Aeson.encode chatMsgMeta

                _ -> liftIO $ putStrLn "Data Message is in binary form."

              liftIO $ loopAndHandleError $ loop chatsTvar


            loopAndHandleError :: ExceptT Text IO () -> IO ()
            loopAndHandleError loop =
              runExceptT loop >>= \case
                Left errStr -> TextIO.putStrLn errStr
                Right _     -> return ()


        -- Update AppState.
        atomically $ writeTVar ( appState ^. #chats ) newChats

        -- Tell user their ID.
        let userIdMsg = UserIdMsg { yourUserId = userId }
        WebSock.sendTextData ( user ^. #conn ) ( Aeson.encode userIdMsg )

        -- Give msg history first.
        let msgHistory = MsgHistory {
          msgs  = chat ^. #msgs
        , users = Map.map ( ^. #name ) users
        , maxJoinCount = chat ^. #config . #maxJoinCount
        }
        WebSock.sendTextData ( user ^. #conn ) ( Aeson.encode msgHistory )

        -- Enter loop.
        WebSock.withPingThread conn 30 ( return () ) $
          flip finally removeUser $ loopAndHandleError $ loop $ appState ^. #chats


getRandomWord :: [ Text ] -> IO Text
getRandomWord wordlist = do
  seedInteger <- fmap seedToInteger seedNew
  let indexInteger = seedInteger `mod` (toInteger $ length wordlist)
      -- Coercion should be safe because the modulus result should be small?
      index = fromIntegral indexInteger
  -- Okay to use !! because it's guaranteed to get an element?
  return $ wordlist !! index


getRandomWords :: Int -> [ Text ]-> IO [ Text ]
getRandomWords n wordlist =
  forM [1..n] (\_ -> getRandomWord wordlist)


mkRandomId :: [ Text ] -> IO Text
mkRandomId wordlist = do
  randomWords <- getRandomWords 6 wordlist
  pure $ Text.intercalate "-" randomWords


getRandomHash :: IO Text
getRandomHash = do
  seed <- seedNew
  let seedStr = show $ seedToInteger seed
      hash    = show $ hashWith SHA256 $ ByteStrC8.pack seedStr
  pure $ Text.pack hash


server :: Servant.ServerT API ( ReaderT AppState Servant.Handler )
server = readLetter
    :<|> writeLetter
    :<|> spawnDispChat
    :<|> spawnPersistChat
    :<|> persistChatEntrance
    :<|> chatHandler


api :: Servant.Proxy API
api = Servant.Proxy


mkApp :: AppState -> Servant.Application
mkApp appState =
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



initApp :: IO AppState
initApp = do

  dbConnPool <- runStderrLoggingT $ createSqlitePool "database.db" 5
  runSqlPool ( runMigration migrateAll ) dbConnPool

  dbLetterMetaEnts :: [ Persist.Entity DbLetterMeta ] <-
    runSqlPool ( selectList [] [] ) dbConnPool

  let dbLetterMetas = fmap Persist.entityVal dbLetterMetaEnts

      splitLetterMeta :: DbLetterMeta -> ( LetterId, LetterMeta )
      splitLetterMeta dbLetterMeta =
        ( dbLetterMeta & dbLetterMetaLetterId
        , dbLetterMeta & dbLetterMetaVal
        )

      idLetterMetaPairs = fmap splitLetterMeta dbLetterMetas

  dbEntranceEnts :: [ Persist.Entity DbEntrance ] <-
    runSqlPool ( selectList [] [] ) dbConnPool

  let dbEntrances = fmap Persist.entityVal dbEntranceEnts

      splitEntrance :: DbEntrance -> ( Text, Entrance )
      splitEntrance dbEntrance =
        ( dbEntrance & dbEntranceEntranceId
        , dbEntrance & dbEntranceVal
        )

      entrances = fmap splitEntrance dbEntrances

  dbChatEnts :: [ Persist.Entity DbChat ] <-
    runSqlPool ( selectList [] [] ) dbConnPool

  let dbChats = fmap Persist.entityVal dbChatEnts

      mkRoom :: DbChat -> ( ChatId, ( Chat, Map Int User ) )
      mkRoom dbChat =
        ( dbChat & dbChatChatId
        -- Room
        , ( dbChat & dbChatVal
          , Map.empty
          )
        )

      rooms = fmap mkRoom dbChats

  wordlist <- fmap lines $ readFile "eff-wordlist.txt"
  initLetterMetas <- atomically $ newTVar $ Map.fromList idLetterMetaPairs
  initEntrances   <- atomically $ newTVar $ Map.fromList entrances
  initRooms       <- atomically $ newTVar $ Map.fromList rooms
  let initAppState = AppState {
      dbConnPool   = dbConnPool
    , letterMetas  = initLetterMetas
    , entrances    = initEntrances
    , chats        = initRooms
    , wordlist     = wordlist
    }
  return initAppState


startApp :: IO ()
startApp = do
  initAppState <- initApp
  Warp.run 9000 $ mkApp initAppState
