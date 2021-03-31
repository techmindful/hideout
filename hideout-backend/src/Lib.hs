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
import           Control.Monad.Reader ( ReaderT, ask, runReaderT )
import           Control.Monad.STM ( atomically )
import           Control.Concurrent.STM.TVar ( TVar, newTVar, readTVar, writeTVar )
import           Data.Function ( (&) )
import qualified Data.Map.Strict as Map
import           Data.Map.Strict ( Map(..) )
import           GHC.Generics ( Generic )


data Letter = Letter
  { body :: String }
  deriving ( Generic, Show )
instance FromJSON Letter
instance ToJSON Letter


type API = "read-letter"  :> Capture "letterId" String :> Get '[ Servant.JSON ] Letter
      :<|> "write-letter" :> ReqBody '[ Servant.JSON ] Letter :> Put '[ Servant.JSON ] NoContent


data AppState = AppState
  { letters :: TVar ( Map String Letter ) }


readLetter :: String -> ReaderT AppState Servant.Handler Letter
readLetter letterId = do
  return (Letter "test")


writeLetter :: Letter -> ReaderT AppState Servant.Handler NoContent
writeLetter letter = do

  liftIO $ putStrLn "write"

  appState <- ask

  liftIO $ do
    oldLetters <- atomically $ readTVar ( appState & letters )
    let newLetters = Map.insert "test_id" letter oldLetters
    atomically $ writeTVar ( appState & letters ) newLetters
    letters <- atomically $ readTVar ( appState & letters )
    putStrLn $ show letters

  return NoContent


server :: Servant.ServerT API (ReaderT AppState Servant.Handler)
server = readLetter
    :<|> writeLetter


api :: Servant.Proxy API
api = Servant.Proxy


app :: AppState -> Servant.Application
app appState =
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
  ( Servant.serve api $ Servant.hoistServer api ( \x -> runReaderT x appState ) server )



startApp :: IO ()
startApp = do
  initLetters <- atomically $ newTVar Map.empty
  let initAppState = AppState { letters = initLetters }
  Warp.run 8080 $ app initAppState
