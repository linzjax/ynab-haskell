{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Client
  ( getUser
  , getBudgets
  , getBudget
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
import Data.Text (Text, pack, unpack, append)
-- import qualified Data.Yaml as Yaml
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Network.HTTP.Simple
  ( setRequestHeader
  , parseRequest
  , httpLBS
  , getResponseStatusCode
  , getResponseHeader
  , getResponseBody)
import System.Environment (lookupEnv)

import Models.User (User(..))
import Models.Budget (BudgetSummaryResponse(..), BudgetDetailResponse(..))

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

getBudgets :: IO (Maybe BudgetSummaryResponse)
getBudgets = do
  response <- getEndpoint "GET https://api.youneedabudget.com/v1/budgets"
  case (getResponseStatusCode response) of
    200 -> return $ decode $ getResponseBody response
    _ -> do
      print "Error"
      return Nothing

getBudget :: Text -> IO (Maybe BudgetDetailResponse)
getBudget budgetId = do
  let getBudgetUrl = "GET https://api.youneedabudget.com/v1/budgets/"
  url <- parseRequest $ unpack $ append getBudgetUrl budgetId
  response <- getEndpoint $ url
  case (getResponseStatusCode response) of
    200 -> return $ decode $ getResponseBody response
    _ -> do
      print "Error"
      return Nothing
