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
import           Crypto.Random ( seedNew, seedToInteger )
import           Crypto.Hash ( SHA256(..), hashWith )

import           Control.Monad.IO.Class
import           Control.Monad.Reader ( ReaderT, ask, runReaderT )
import           Control.Monad.STM ( atomically )
import           Control.Concurrent.STM.TVar ( TVar, newTVar, readTVar, writeTVar )
import qualified Data.ByteString.Char8 as ByteStrC8
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
      :<|> "write-letter" :> ReqBody '[ Servant.JSON ] Letter :> Put '[ Servant.JSON ] String


data AppState = AppState
  { letters :: TVar ( Map String Letter ) }


readLetter :: String -> ReaderT AppState Servant.Handler Letter
readLetter letterId = do

  appState <- ask

  letters <- liftIO $ atomically $ readTVar ( appState & letters )

  let maybeLetter = Map.lookup letterId letters
  case maybeLetter of
    Nothing -> Servant.throwError Servant.err404
    Just letter ->
      return letter



writeLetter :: Letter -> ReaderT AppState Servant.Handler String
writeLetter letter = do

  liftIO $ putStrLn "write"

  appState <- ask

  letterId <- liftIO $ do

    oldLetters <- atomically $ readTVar ( appState & letters )

    -- Get a random hash.
    seed <- seedNew
    let seedStr = show $ seedToInteger seed
        hash    = show $ hashWith SHA256 $ ByteStrC8.pack seedStr

    -- Insert new letter into AppState.
    let newLetters = Map.insert hash letter oldLetters
    atomically $ writeTVar ( appState & letters ) newLetters

    -- Debug print
    letters <- atomically $ readTVar ( appState & letters )
    putStrLn $ "Current letters:\n----------\n" ++  show letters

    return hash

  return letterId


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
