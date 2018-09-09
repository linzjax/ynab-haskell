{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Client
  ( getUser
  , getBudgets
  , getBudgetById
  , getBudgetSettingsById
  , getAccounts
  , getAccountById
  , getCategories
  , getCategoryById
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (MonadThrow(..))
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
import qualified Data.ByteString.Lazy.Internal as L
import Data.Maybe (fromMaybe)
-- import qualified Data.HashMap as M
import Data.HashMap.Strict (toList)
import qualified Data.Text as T
-- import qualified Data.Yaml as Yaml
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Network.HTTP.Simple
  ( setRequestHeader
  , parseRequest
  , Response(..)
  , Request(..)
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
import Models.Category
  ( CategoriesResponse(..), CategoryResponse(..))
import Models.YnabError (YnabError(..))

-- | Helpers for processesing API requests

-- | Formats the full API call: url, authorization, etc.
formatEndpoint :: Request -> IO (Response L.ByteString)
formatEndpoint requestUrl = do
  apiKey <- S8.pack . fromMaybe "" <$> lookupEnv "API_TOKEN"
  let req = setRequestHeader "Authorization" ["Bearer " <> apiKey] $ requestUrl
  response <- httpLBS req
  return response

-- | Formats the request url, including id variables, etc.
-- | ex: ["GET", bId, "accounts"] -> GET "v1/budgets/<budget_id>/accounts"
formatUrl :: (MonadThrow m) => [T.Text] -> m Request
formatUrl urls = parseRequest
               . T.unpack . T.append requestType
               . T.append baseUrl
               . T.intercalate "/" $ endpoints
    where baseUrl = " https://api.youneedabudget.com/v1/budgets/"
          requestType = head urls
          endpoints = tail urls

-- | Parses the response to check if the status came back 200.
-- | If it did, try to parse into the appropriate Data Type.
-- | If it did not, try to parse into the YnabError Data Type.
processResponse :: (FromJSON b, Monad m)
              => Response L.ByteString -> m (Either YnabError b)
processResponse response =
  case (getResponseStatusCode response) of
    200 -> do
      case (decode $ getResponseBody response) of
        Just res -> return $ Right res
        Nothing  -> return $ Left parseError
    _ -> do
      case (decode $ getResponseBody response) of
        Just err -> return $ Left err
        Nothing  -> return $ Left parseError
  where parseError = (YnabError "500" "WHOOPS"
                          "An error occured while parsing the\
                          \ error. Ironic, I know.")

-- | Strings together formatting the url, putting together the request, and
-- | finally processing the request.
processRequest :: (FromJSON b) => [T.Text] -> IO (Either YnabError b)
processRequest url = formatUrl url >>= formatEndpoint >>= processResponse

type BudgetId = T.Text
type AccountId = T.Text
type CategoryId = T.Text

-- | All endpoints for YNAB's API
getUser :: IO (Either YnabError User)
getUser = formatEndpoint "GET https://api.youneedabudget.com/v1/user"
      >>= processResponse

getBudgets :: IO (Either YnabError BudgetSummaryResponse)
getBudgets = processRequest ["GET"]

getBudgetById :: BudgetId -> IO (Either YnabError BudgetDetailResponse)
getBudgetById bId = processRequest ["GET", bId]

getBudgetSettingsById :: BudgetId -> IO (Either YnabError BudgetSettings)
getBudgetSettingsById bId = processRequest ["GET", bId, "settings"]

getAccounts :: BudgetId -> IO (Either YnabError AccountsSummaryResponse)
getAccounts bId = processRequest ["GET", bId, "accounts"]

getAccountById :: BudgetId -> AccountId -> IO (Either YnabError AccountDetailResponse)
getAccountById bId aId = processRequest ["GET", bId, "accounts", aId]

getCategories :: BudgetId -> IO (Either YnabError CategoriesResponse)
getCategories bId = processRequest ["GET", bId, "categories"]

getCategoryById :: BudgetId -> CategoryId -> IO (Either YnabError CategoryResponse)
getCategoryById bId cId = processRequest ["GET", bId, "categories", cId]


-- getPayees - budgetId
-- getPayeeById - budgetId, payeeId
--
-- getPayeeLocations - budgetId
-- getPayeeLocationById - budgetId, payeeLocationId
-- getPayeeLocationByPayee - budgetId, payeeId
--
-- getBudgetMonths - budgetId
-- getBudgetMonth - budgetId, Month
--
-- getTransactions - budgetId
-- getTransactionsByAccount - budgetId accountId
-- getTransactionsByCategory - budgetId categoryId
-- getTransactionById - budgetId transactionId
--
-- updateTransaction - budgetId transactionId Transaction
-- createTrasnaction - budgetId Transaction
-- bulkCreateTransactions - budgetId [Transaction]
--
-- getScheduledTransactions - budgetId
-- getScheduledTransactionById - budgetId, scheduleTransactionId
