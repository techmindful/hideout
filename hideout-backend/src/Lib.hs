{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  { body     :: String
  , maxReads :: Int
  }
  deriving ( Generic, Show )
instance FromJSON Letter
instance ToJSON Letter


data LetterMeta = LetterMeta
  { letter  :: Letter
  , reads   :: Int
  }
  deriving ( Generic, Show )
instance FromJSON LetterMeta
instance ToJSON LetterMeta


type API = "read-letter"  :> Capture "letterId" String :> Get '[ Servant.JSON ] LetterMeta
      :<|> "write-letter" :> ReqBody '[ Servant.JSON ] Letter :> Put '[ Servant.JSON ] String


data AppState = AppState
  { letterMetas :: TVar ( Map String LetterMeta ) }


readLetter :: String -> ReaderT AppState Servant.Handler LetterMeta
readLetter letterId = do

  appState <- ask

  letterMetas <- liftIO $ atomically $ readTVar ( appState & letterMetas )

  let maybeLetterMeta = Map.lookup letterId letterMetas
  case maybeLetterMeta of
    Nothing -> Servant.throwError Servant.err404
    Just letterMeta ->
      return letterMeta



writeLetter :: Letter -> ReaderT AppState Servant.Handler String
writeLetter letter = do

  appState <- ask

  letterId <- liftIO $ do

    oldLetterMetas <- atomically $ readTVar ( appState & letterMetas )

    -- Get a random hash.
    seed <- seedNew
    let seedStr = show $ seedToInteger seed
        hash    = show $ hashWith SHA256 $ ByteStrC8.pack seedStr

    -- Create a new LetterMeta, and insert it into AppState.
    let newLetterMeta  = LetterMeta { letter = letter, reads = 0 }
        newLetterMetas = Map.insert hash newLetterMeta oldLetterMetas
    atomically $ writeTVar ( appState & letterMetas ) newLetterMetas

    -- Debug print
    letterMetas <- atomically $ readTVar ( appState & letterMetas )
    putStrLn $ "Current letter metas:\n----------\n" ++  show letterMetas

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
  initLetterMetas <- atomically $ newTVar Map.empty
  let initAppState = AppState { letterMetas = initLetterMetas }
  Warp.run 8080 $ app initAppState
