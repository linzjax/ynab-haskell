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
  , getPayees
  , getPayeeById
  , getPayeeLocations
  , getPayeeLocationById
  , getPayeeLocationByPayee
  , getBudgetMonths
  , getBudgetMonth
  , getTransactions
  , getTransactionsByAccount
  , getTransactionsByCategory
  , getTransactionsByPayee
  , getTransactionById
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
  , BudgetSettings(..)
  , BudgetId )
import Models.Account
  ( AccountsSummaryResponse(..)
  , AccountDetailResponse(..))
import Models.Category
  ( CategoriesResponse(..), CategoryResponse(..))
import Models.Payee
  ( PayeesResponse(..)
  , PayeeResponse(..)
  , PayeeId
  , PayeeLocationsResponse(..)
  , PayeeLocationResponse(..)
  , PayeeLocationId )
import Models.Month
  ( MonthSummariesResponse(..)
  , MonthDetailResponse(..)
  )
import Models.Transaction
  ( TransactionsResponse(..)
  , TransactionResponse(..)
  , TransactionId
  )
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
getPayees :: BudgetId -> IO (Either YnabError PayeesResponse)
getPayees bId = processRequest ["GET", bId, "payees"]

-- getPayeeById - budgetId, payeeId
getPayeeById :: BudgetId -> PayeeId -> IO (Either YnabError PayeeResponse)
getPayeeById bId pId = processRequest ["GET", bId, "payees", pId]

-- getPayeeLocations - budgetId
getPayeeLocations :: BudgetId -> IO (Either YnabError PayeeLocationsResponse)
getPayeeLocations bId = processRequest ["GET", bId, "payee_locations"]

-- getPayeeLocationById - budgetId, payeeLocationId
getPayeeLocationById :: BudgetId -> PayeeLocationId -> IO (Either YnabError PayeeLocationResponse)
getPayeeLocationById bId plId= processRequest ["GET", bId, "payee_locations", plId]

-- getPayeeLocationByPayee - budgetId, payeeId
getPayeeLocationByPayee :: BudgetId -> PayeeId -> IO (Either YnabError PayeeLocationResponse)
getPayeeLocationByPayee bId pId = processRequest ["GET", bId, "payee_locations", pId]

-- getBudgetMonths - budgetId
getBudgetMonths :: BudgetId -> IO (Either YnabError MonthSummariesResponse)
getBudgetMonths bId = processRequest ["GET", bId, "months"]

-- getBudgetMonth - budgetId, Month
getBudgetMonth :: BudgetId -> T.Text -> IO (Either YnabError MonthDetailResponse)
getBudgetMonth bId month = processRequest ["GET", bId, "months", month]

-- getTransactions - budgetId
getTransactions :: BudgetId -> IO (Either YnabError TransactionsResponse)
getTransactions bId = processRequest ["GET", bId, "transactions"]

-- getTransactionsByAccount - budgetId accountId
getTransactionsByAccount :: BudgetId -> AccountId -> IO (Either YnabError TransactionsResponse)
getTransactionsByAccount bId aId = processRequest ["GET", bId, "accounts", aId, "transactions"]

-- getTransactionsByCategory - budgetId categoryId
getTransactionsByCategory :: BudgetId -> CategoryId -> IO (Either YnabError TransactionsResponse)
getTransactionsByCategory bId cId = processRequest ["GET", bId, "categories", cId, "transactions"]

-- getTransactionsByPayee - budgetId payeeId
getTransactionsByPayee :: BudgetId -> PayeeId -> IO (Either YnabError TransactionsResponse)
getTransactionsByPayee bId pId = processRequest ["GET", bId, "payees", pId, "transactions"]

-- getTransactionById - budgetId transactionId
getTransactionById :: BudgetId -> TransactionId -> IO (Either YnabError TransactionResponse)
getTransactionById bId tId = processRequest ["GET", bId, "transactions", tId]

--
-- updateTransaction - budgetId transactionId Transaction
-- createTrasnaction - budgetId Transaction
-- bulkCreateTransactions - budgetId [Transaction]
--
-- getScheduledTransactions - budgetId
-- getScheduledTransactionById - budgetId, scheduleTransactionId
