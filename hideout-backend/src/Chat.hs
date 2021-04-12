{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}

module Chat where

import qualified Network.WebSockets as WebSock

import           Data.Aeson ( FromJSON, ToJSON )

import           GHC.Generics ( Generic )


data Msg = Msg
  { msgType :: String
  , msgBody :: String
  }
  deriving ( Generic, Show )
instance FromJSON Msg
instance ToJSON   Msg


newtype ChatId = ChatId { unChatId :: String }
  deriving ( Eq, Generic, Ord, Show )
instance FromJSON ChatId
instance ToJSON   ChatId


data UserId = UserId
  { chatId :: ChatId
  , index  :: Int
  } deriving ( Eq, Generic, Ord, Show )
instance FromJSON UserId
instance ToJSON   UserId


data ChatUser = ChatUser
  { userId :: UserId
  , name   :: String
  , userConn   :: WebSock.Connection
  }


data Chat = Chat
  { users :: [ ChatUser ]
  , msgs :: [ Msg ]
  , maxJoinCount :: Int
  , joinCount :: Int
  } deriving ( Generic )

