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
import           Data.Text ( Text )
import           GHC.Generics ( Generic )


data MsgFromClient = MsgFromClient
  { msgType :: String
  , msgBody :: String
  } deriving ( Generic, Read, Show )
instance FromJSON MsgFromClient
instance ToJSON   MsgFromClient


data ChatMsgMeta = ChatMsgMeta
  { msgFromClient :: MsgFromClient
  , userId :: Int
  , username :: String
  , posixTimeSec :: Int
  } deriving ( Generic, Read, Show )
instance FromJSON ChatMsgMeta
instance ToJSON   ChatMsgMeta


data CtrlMsg = CtrlMsg
  { msgType :: String
  , msgBody :: String
  } deriving ( Generic )
instance FromJSON CtrlMsg
instance ToJSON   CtrlMsg


newtype ChatId = ChatId { unChatId :: Text }
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


data Entrance = Entrance
  { chatId       :: ChatId
  , maxViewCount :: Int
  , viewCount    :: Int
  } deriving ( Generic, Read, Show )
derivePersistField "Entrance"


data Chat = Chat
  { msgs :: [ ChatMsgMeta ]
  , joinCount :: Int
  , config :: Config
  } deriving ( Generic, Read, Show )
derivePersistField "Chat"


data MsgHistory = MsgHistory
  { msgs  :: [ ChatMsgMeta ]
  , users :: Map Int String
  , maxJoinCount :: Maybe Int
  } deriving ( Generic )
instance ToJSON MsgHistory 


data UserIdMsg = UserIdMsg
  { yourUserId :: Int }
  deriving ( Generic )
instance ToJSON UserIdMsg


notFoundCtrlMsg :: CtrlMsg
notFoundCtrlMsg =
  CtrlMsg {
    msgType = "err"
  , msgBody = "notFound"
  }


maxJoinedCtrlMsg :: CtrlMsg
maxJoinedCtrlMsg =
  CtrlMsg {
    msgType = "err"
  , msgBody = "maxJoined"
  }

