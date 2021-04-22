{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedLabels #-}
{-# language TemplateHaskell #-}

module Chat where

import qualified Network.WebSockets as WebSock

import           Database.Persist.TH ( derivePersistField )

import           Control.Lens ( (^.), (.~), (%~) )
import           Data.Aeson ( FromJSON, ToJSON )
import           Data.Map.Strict ( Map )
import           GHC.Generics ( Generic )


data MsgFromClient = MsgFromClient
  { msgType :: String
  , msgBody :: String
  } deriving ( Generic, Read, Show )
instance FromJSON MsgFromClient
instance ToJSON   MsgFromClient


data MsgFromServer = MsgFromServer
  { msgFromClient :: MsgFromClient
  , userId :: Int
  , username :: String
  , posixTimeSec :: Int
  } deriving ( Generic, Read, Show )
instance FromJSON MsgFromServer
instance ToJSON   MsgFromServer


newtype ChatId = ChatId { unChatId :: String }
  deriving ( Eq, Generic, Ord, Read, Show )
instance FromJSON ChatId
instance ToJSON   ChatId
derivePersistField "ChatId"


data User = User
  { name :: String
  , conn :: WebSock.Connection
  } deriving ( Generic )


data Config = Config
  { maxJoinCount :: Maybe Int
  , expiry :: Expiry
  , persist :: Bool
  , sendHistory :: Bool
  } deriving ( Generic, Read, Show )
instance FromJSON Config


data Expiry
  = Empty
  | MaxJoined
  | Never
  deriving ( Generic, Read, Show )
instance FromJSON Expiry


data Chat = Chat
  { msgs :: [ MsgFromServer ]
  , joinCount :: Int
  , config :: Config
  } deriving ( Generic, Read, Show )
derivePersistField "Chat"


data MsgHistory = MsgHistory
  { msgs  :: [ MsgFromServer ]
  , users :: Map Int String
  , maxJoinCount :: Maybe Int
  } deriving ( Generic )
instance ToJSON MsgHistory 


data UserIdMsg = UserIdMsg
  { yourUserId :: Int }
  deriving ( Generic )
instance ToJSON UserIdMsg

