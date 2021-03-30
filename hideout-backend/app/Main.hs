{-# language DataKinds #-}
{-# language TypeOperators #-}

module Main where

import qualified Data.Aeson
import qualified Servant
import           Servant ( (:>), Capture, Get )


type API = "read-letter" :> Capture "letterId" String :> Get '[Servant.JSON] Letter


type Letter = String


main :: IO ()
main = putStrLn "hi"
