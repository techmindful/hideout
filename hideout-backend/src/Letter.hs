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

  Letter
    body String
    maxReadCount Int
    deriving Generic Show

  LetterMeta
    letter Letter
    readCount Int
    deriving Generic Show

|]
instance FromJSON Letter
instance ToJSON Letter
instance FromJSON LetterMeta
instance ToJSON LetterMeta

