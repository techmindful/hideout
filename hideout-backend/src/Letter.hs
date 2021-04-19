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

module Letter where

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

  Letter json
    body String
    maxReadCount Int
    deriving Generic Show

  LetterMeta json
    letter Letter
    readCount Int
    deriving Generic Show

  LetterMetaWithId
    id' String
    val LetterMeta
    Id' id'  -- Uniqueness Constraint
    deriving Show

|]


{-
data Letter = Letter
  { body :: String
  , maxReadCount :: Int
  } deriving ( Generic, Show )
instance FromJSON Letter
instance ToJSON   Letter


data LetterMeta = LetterMeta
  { letter :: Letter
  , readCount :: Int
  } deriving ( Generic, Show )
instance FromJSON LetterMeta
instance ToJSON   LetterMeta
-}

