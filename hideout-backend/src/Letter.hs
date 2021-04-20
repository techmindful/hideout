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

import           Database.Persist.TH ( derivePersistField )

import qualified Data.Aeson as Aeson
import           Data.Aeson ( FromJSON, ToJSON )
import           GHC.Generics ( Generic )


data Letter = Letter
  { body :: String
  , maxReadCount :: Int
  , persist :: Bool
  } deriving ( Generic, Read, Show )
instance FromJSON Letter
instance ToJSON   Letter


data LetterMeta = LetterMeta
  { letter :: Letter
  , readCount :: Int
  } deriving ( Generic, Read, Show )
instance FromJSON LetterMeta
instance ToJSON   LetterMeta

derivePersistField "Letter"
derivePersistField "LetterMeta"

