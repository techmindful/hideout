{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( startApp
    , app
    ) where
    
import qualified Servant
import           Servant ( (:>), (:<|>)(..), Capture, Get, NoContent(..), Put, ReqBody )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.Cors ( cors, CorsResourcePolicy(..) )

import           Data.Aeson ( FromJSON, ToJSON )

import           Control.Monad.IO.Class
import           GHC.Generics ( Generic )


data Letter = Letter
  { body :: String }
  deriving ( Generic )
instance FromJSON Letter
instance ToJSON Letter


type API = "read-letter"  :> Capture "letterId" String :> Get '[ Servant.JSON ] Letter
      :<|> "write-letter" :> ReqBody '[ Servant.JSON ] Letter :> Put '[ Servant.JSON ] NoContent


readLetter :: String -> Servant.Handler Letter
readLetter letterId = do
  return (Letter "test")


writeLetter :: Letter -> Servant.Handler NoContent
writeLetter letter = do
  liftIO $ putStrLn "write"
  return NoContent


server :: Servant.Server API
server = readLetter
    :<|> writeLetter


api :: Servant.Proxy API
api = Servant.Proxy


app :: Servant.Application
app =
  cors ( \_ -> Just $ CorsResourcePolicy
          { corsOrigins = Nothing
          , corsMethods = [ "GET", "PUT" ]
          , corsRequestHeaders = [ "Content-Type" ]
          , corsExposedHeaders = Nothing
          , corsMaxAge = Nothing  -- TODO: Change in production
          , corsVaryOrigin = True
          , corsRequireOrigin = False
          , corsIgnoreFailures = False
          }
       )
   ( Servant.serve api server )



startApp :: IO ()
startApp = Warp.run 8080 app
