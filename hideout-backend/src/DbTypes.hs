{-# LANGUAGE DataKinds #-}           
{-# language DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}  
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}   
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}  
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DbTypes where

import           Chat
import           Letter

import           Database.Persist.TH
  ( mkMigrate
  , mkPersist
  , persistLowerCase
  , share
  , sqlSettings
  )

import qualified Data.Aeson as Aeson
import           Data.Aeson ( FromJSON, ToJSON )
import           GHC.Generics ( Generic )


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

  DbLetterMeta
    letterId String
    val LetterMeta
    UniqueLetterId letterId  -- Uniqueness Constraint
    deriving Show

  DbEntrance
    entranceId String
    val Entrance
    UniqueEntranceId entranceId

  DbChat
    chatId ChatId
    val Chat
    UniqueChatId chatId  -- Uniqueness Constraint
    deriving Show

|]

