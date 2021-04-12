{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}

module Chat where

import qualified Network.WebSockets as WebSock

import           Data.Aeson ( FromJSON, ToJSON )

import           Data.Map.Strict ( Map )
import           GHC.Generics ( Generic )


data MsgFromClient = MsgFromClient
  { msgType :: String
  , msgBody :: String
  }
  deriving ( Generic, Show )
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
  , userConn :: WebSock.Connection
  }


data Chat = Chat
  { users :: Map Int User
  , msgs :: [ MsgFromClient ]
  , maxJoinCount :: Int
  , joinCount :: Int
  } deriving ( Generic )

