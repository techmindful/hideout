{-# LANGUAGE DataKinds #-}           
{-# language DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}  
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}   
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}  
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Letter where

import qualified Data.Aeson as Aeson
import           Data.Aeson ( FromJSON, ToJSON )
import qualified Data.Text as Text
import           Data.Text ( Text )
import           Database.Persist.TH ( derivePersistField )
import           GHC.Generics ( Generic )
import qualified Servant


newtype LetterId = LetterId { unLetterId :: Text }
  deriving ( Eq, Generic, Ord, Read, Show )
instance FromJSON LetterId
instance ToJSON   LetterId
derivePersistField "LetterId"
instance Servant.FromHttpApiData LetterId where
  parseUrlPiece :: Text -> Either Text LetterId
  parseUrlPiece = Right . LetterId


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

