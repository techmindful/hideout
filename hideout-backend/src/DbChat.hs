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

module DbChat where

import           Chat

import           Database.Persist.TH
  ( mkMigrate
  , mkPersist
  , persistLowerCase
  , share
  , sqlSettings
  )

{-
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

  DbChat
    id' ChatId
    val Chat
    Id' id'  -- Uniqueness Constraint
    deriving Show

|]
-}
