{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Client
  ( getUser
  , getBudgets
  , getBudget
  , getBudgetSettings
  , getAccounts
  , getAccount
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
  , Response(..)
  , httpLBS
  , getResponseStatusCode
  , getResponseHeader
  , getResponseBody)
import System.Environment (lookupEnv)

import Models.User (User(..))
import Models.Budget
  ( BudgetSummaryResponse(..)
  , BudgetDetailResponse(..)
  , BudgetSettings(..))
import Models.Account
  ( AccountsSummaryResponse(..)
  , AccountDetailResponse(..))
import Models.YnabError (YnabError(..))

-- getEndpoint :: Request -> Response
getEndpoint requestUrl = do
  apiKey <- S8.pack . fromMaybe "" <$> lookupEnv "API_TOKEN"
  let req = setRequestHeader "Authorization" ["Bearer " <> apiKey] $ requestUrl
  response <- httpLBS req
  return response

parseError = (YnabError "500" "WHOOPS"
                        "An error occured while parsing the\
                        \ error. Ironic, I know.")


getUser :: IO (Either YnabError User)
getUser = do
  response <- getEndpoint "GET https://api.youneedabudget.com/v1/user"
  case (getResponseStatusCode response) of
    200 -> do
      case (decode $ getResponseBody response) of
        Just res -> return $ Right res
        Nothing  -> return $ Left parseError
    _ -> do
      case (decode $ getResponseBody response) of
        Just err -> return $ Left err
        Nothing  -> return $ Left parseError

getBudgets :: IO (Either YnabError BudgetSummaryResponse)
getBudgets = do
  response <- getEndpoint "GET https://api.youneedabudget.com/v1/budgets"
  case (getResponseStatusCode response) of
    200 -> do
      case (decode $ getResponseBody response) of
        Just res -> return $ Right res
        Nothing  -> return $ Left parseError
    _ -> do
      case (decode $ getResponseBody response) of
        Just err -> return $ Left err
        Nothing  -> return $ Left parseError

getBudget :: Text -> IO (Either YnabError BudgetDetailResponse)
getBudget bId = do
  let getBudgetUrl = "GET https://api.youneedabudget.com/v1/budgets/"
  url <- parseRequest $ unpack $ append getBudgetUrl bId
  response <- getEndpoint $ url
  case (getResponseStatusCode response) of
    200 -> do
      case (decode $ getResponseBody response) of
        Just res -> return $ Right res
        Nothing  -> return $ Left parseError
    _ -> do
      case (decode $ getResponseBody response) of
        Just err -> return $ Left err
        Nothing  -> return $ Left parseError

getBudgetSettings :: Text -> IO (Either YnabError BudgetSettings)
getBudgetSettings bId = do
  let getBudgetUrl = append "GET https://api.youneedabudget.com/v1/budgets/" bId
  url <- parseRequest $ unpack $ append getBudgetUrl "/settings"
  response <- getEndpoint $ url
  print response
  case (getResponseStatusCode response) of
    200 -> do
      case (decode $ getResponseBody response) of
        Just res -> return $ Right res
        Nothing  -> return $ Left parseError
    _ -> do
      case (decode $ getResponseBody response) of
        Just err -> return $ Left err
        Nothing  -> return $ Left parseError

getAccounts :: Text -> IO (Either YnabError AccountsSummaryResponse)
getAccounts bId = do
  let getBudgetUrl = append "GET https://api.youneedabudget.com/v1/budgets/" bId
  url <- parseRequest $ unpack $ append getBudgetUrl "/accounts"
  response <- getEndpoint $ url
  print response
  case (getResponseStatusCode response) of
    200 -> do
      case (decode $ getResponseBody response) of
        Just res -> return $ Right res
        Nothing  -> return $ Left parseError
    _ -> do
      case (decode $ getResponseBody response) of
        Just err -> return $ Left err
        Nothing  -> return $ Left parseError

getAccount :: Text -> Text -> IO (Either YnabError AccountDetailResponse)
getAccount bId aId = do
  let getBudgetUrl = append "GET https://api.youneedabudget.com/v1/budgets/" bId
  let getAccountUrl = append getBudgetUrl "/accounts/"
  url <- parseRequest $ unpack $ append getAccountUrl aId
  response <- getEndpoint $ url
  print response
  case (getResponseStatusCode response) of
    200 -> do
      case (decode $ getResponseBody response) of
        Just res -> return $ Right res
        Nothing  -> return $ Left parseError
    _ -> do
      case (decode $ getResponseBody response) of
        Just err -> return $ Left err
        Nothing  -> return $ Left parseError
