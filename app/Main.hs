{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Network.HTTP.Simple
import System.Environment (lookupEnv)

main :: IO ()
main = do
  apiKey <- S8.pack . fromMaybe "" <$> lookupEnv "API_TOKEN"
  liftIO $ print apiKey
  let request = setRequestHeader "Authorization" [apiKey]
               $ "GET https://api.youneedabudget.com/v1/user"
  response <- httpJSON request

  putStrLn $ "The status code was: " ++
             show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
