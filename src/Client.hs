{-# LANGUAGE OverloadedStrings #-}

module Client
  ( getUser
  , getBudgets
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( Value
  , FromJSON(..)
  , Object(..)
  , Value(..)
  , decode
  , withObject
  , (.:)
  )
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe (fromMaybe)
-- import qualified Data.HashMap as M
import Data.HashMap.Strict (toList)
import Data.Text (Text)
-- import qualified Data.Yaml as Yaml
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import GHC.Generics (Generic)
import Network.HTTP.Simple (setRequestHeader, httpLBS, getResponseStatusCode, getResponseHeader, getResponseBody)
import System.Environment (lookupEnv)

import Models.Common (UserResponse(..), YnabResponse(..))
import Models.User (User(..))
import Models.Budget (BudgetList(..))

-- getEndpoint :: Request -> Response
getEndpoint requestUrl = do
  apiKey <- S8.pack . fromMaybe "" <$> lookupEnv "API_TOKEN"
  let req = setRequestHeader "Authorization" ["Bearer " <> apiKey] $ requestUrl
  response <- httpLBS req
  return response

getUser :: IO (Maybe User)
getUser = do
  response <- getEndpoint "GET https://api.youneedabudget.com/v1/user"
  case (getResponseStatusCode response) of
    200 -> return $ decode $ getResponseBody response
    _ -> do
      print "Error"
      return Nothing

-- getBudgets :: IO ()
getBudgets :: IO (Maybe BudgetList)
getBudgets = do
  response <- getEndpoint "GET https://api.youneedabudget.com/v1/budgets"
  print response
  case (getResponseStatusCode response) of
    200 -> return $ decode $ getResponseBody response
    _ -> do
      print "Error"
      return Nothing
