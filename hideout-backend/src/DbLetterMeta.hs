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

module DbLetterMeta where

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
    id' String
    val LetterMeta
    Id' id'  -- Uniqueness Constraint
    deriving Show

|]

