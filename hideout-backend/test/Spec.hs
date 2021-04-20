{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main ( main ) where

import           App ( API(..), api, initApp, mkApp )
import           Letter

import qualified Test.Hspec as Hspec
import           Test.Hspec ( Spec, hspec, it, shouldBe )
import qualified Test.Hspec.Wai
import qualified Test.Hspec.Wai.JSON

import           Network.HTTP.Client ( newManager, defaultManagerSettings )
import qualified Servant
import           Servant ( (:<|>)(..) )
import           Servant.API.WebSocket ( WebSocket )
import           Servant.Client
  ( ClientM
  , baseUrlPort
  , client
  , mkClientEnv
  , parseBaseUrl
  , runClientM
  )
import qualified Network.Wai.Handler.Warp as Warp


withUserApp :: ( Warp.Port -> IO () ) -> IO ()
withUserApp action = do
  initAppState <- initApp
  let app = mkApp initAppState
  Warp.testWithApplication ( pure app ) action


readLetter  :: String -> ClientM LetterMeta
writeLetter :: Letter -> ClientM String
spawnDispChat :: String -> ClientM String
spawnPersistChat :: String -> ClientM String
chat :: String -> WebSocket
( readletter :<|>
  writeLetter :<|>
  spawnDispChat :<|>
  spawnPersistChat :<|>
  chat
  ) = client ( Servant.Proxy API )


spec :: Spec
spec = do

  Hspec.around withUserApp $ do

    baseUrl <- Hspec.runIO $ parseBaseUrl "http://localhost"
    manager <- Hspec.runIO $ newManager defaultManagerSettings

    let clientEnv port = mkClientEnv manager ( baseUrl { baseUrlPort = port } )

    Hspec.describe "Letter" $ do
      it "should write a new letter" $ \port -> do
        let letter = Letter {
              body = "Test letter"
            , maxReadCount = 3
            }
        letterId <- runClientM ( writeLetter letter ) ( clientEnv port )
        fmap length letterId `shouldBe` Right 64
        

main :: IO ()
main = hspec spec
