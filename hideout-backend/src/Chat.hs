{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedLabels #-}

module Chat where

import qualified Network.WebSockets as WebSock

import           Control.Lens ( (^.), (.~), (%~) )
import           Data.Aeson ( FromJSON, ToJSON )
import           Data.Map.Strict ( Map )
import           GHC.Generics ( Generic )


data MsgFromClient = MsgFromClient
  { msgType :: String
  , msgBody :: String
  } deriving ( Generic, Show )
instance FromJSON MsgFromClient
instance ToJSON   MsgFromClient


data MsgFromServer = MsgFromServer
  { msgFromClient :: MsgFromClient
  , username :: String
  } deriving ( Generic, Show )
instance FromJSON MsgFromServer
instance ToJSON   MsgFromServer


newtype ChatId = ChatId { unChatId :: String }
  deriving ( Eq, Generic, Ord, Show )
instance FromJSON ChatId
instance ToJSON   ChatId


data User = User
  { name :: String
  , conn :: WebSock.Connection
  } deriving ( Generic )


data Chat = Chat
  { msgs :: [ MsgFromServer ]
  , joinCount :: Int
  , config :: Config
  } deriving ( Generic )


data Config = Config
  { maxJoinCount :: Maybe Int
  , expiry :: Expiry
  , persist :: Bool
  , sendHistory :: Bool
  } deriving ( Generic )
instance FromJSON Config


data Expiry
  = Empty
  | MaxJoined
  | Never
  deriving ( Generic )
instance FromJSON Expiry


data MsgHistory = MsgHistory
  { msgs  :: [ MsgFromServer ]
  , users :: [ String ]
  } deriving ( Generic )
instance ToJSON MsgHistory 

