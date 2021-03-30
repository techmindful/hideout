{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where
    
import qualified Servant
import           Servant ( (:>), Capture, Get )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import           Data.Aeson ( FromJSON, ToJSON )

import           GHC.Generics ( Generic )


data Letter = Letter String
  deriving ( Generic )
instance FromJSON Letter
instance ToJSON Letter


type API = "read-letter" :> Capture "letterId" String :> Get '[ Servant.JSON ] Letter


testLetter :: String -> Servant.Handler Letter
testLetter letterId = return (Letter "test")


server :: Servant.Server API
server = testLetter


api :: Servant.Proxy API
api = Servant.Proxy


app :: Servant.Application
app = Servant.serve api server


startApp :: IO ()
startApp = Warp.run 8080 app
