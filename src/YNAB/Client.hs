{-# LANGUAGE OverloadedStrings #-}

module YNAB.Client
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
  , postTransaction
  , postTransactions
  , updateTransaction
  , getScheduledTransactions
  , getScheduledTransactionById
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , decode
  )
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Simple
  ( setRequestHeader
  , parseRequest
  , Response
  , Request
  , httpLBS
  , setRequestBodyJSON
  , getResponseStatusCode
  , getResponseBody)
import System.Environment (lookupEnv)

import YNAB.Models.User
import YNAB.Models.Budget
import YNAB.Models.Account
import YNAB.Models.Category
import YNAB.Models.Config
import YNAB.Models.Payee
import YNAB.Models.Month
import YNAB.Models.Transaction
import YNAB.Models.ScheduledTransaction
import YNAB.Models.YnabError

-- | Helpers for processesing API requests

-- | Formats full GET API calls: url, authorization, etc.
formatGetRequest :: YnabConfig -> Request -> IO (Response L.ByteString)
formatGetRequest config requestUrl = do
  let apiKey = getKey config
  let req = setRequestHeader "Authorization" ["Bearer " <> apiKey] requestUrl
  httpLBS req

-- | Formats full POST API calls: url, authorization, body, etc.
formatPostRequest :: (ToJSON a) => YnabConfig -> a -> Request ->
                                   IO (Response L.ByteString)
formatPostRequest config body requestUrl = do
  let apiKey = getKey config
  let req = setRequestHeader "Authorization" ["Bearer " <> apiKey] $
              setRequestBodyJSON body requestUrl
  httpLBS req

-- | Formats the request url, including id variables, etc.
-- | ex: ["GET", bId, "accounts"] -> GET "v1/budgets/<budget_id>/accounts"
formatUrl :: (MonadThrow m) => YnabConfig -> [T.Text] -> m Request
formatUrl config urls = parseRequest
                        . T.unpack . T.append requestType
                        . T.append baseUrl
                        . T.append bId
                        . T.append "/"
                        . T.intercalate "/" $ endpoints
    where baseUrl = " https://api.youneedabudget.com/v1/budgets/"
          bId = fromMaybe "" $ configBudgetId config
          requestType = head urls
          endpoints = tail urls

-- | Parses the response to check if the status came back 200.
-- | If it did, try to parse into the appropriate Data Type.
-- | If it did not, try to parse into the YnabError Data Type.
processResponse :: (FromJSON b)
              => Response L.ByteString -> IO (Either YnabError b)
processResponse response =
  case getResponseStatusCode response of
    200 ->
      case decode $ getResponseBody response of
        Just res -> return $ Right res
        Nothing  -> return $ Left parseError
    201 ->
      case decode $ getResponseBody response of
        Just res -> return $ Right res
        Nothing  -> return $ Left parseError
    _ ->
      case decode $ getResponseBody response of
        Just err -> return $ Left err
        Nothing  -> return $ Left parseError
  where parseError = YnabError "500" "WHOOPS"
                          "An error occured while parsing the\
                          \ error. Ironic, I know."

-- | Strings together formatting the url, putting together the request, and
-- | finally processing the request.
processRequest :: (FromJSON b) => YnabConfig -> [T.Text] -> IO (Either YnabError b)
processRequest config url =
  formatUrl config url >>= formatGetRequest config >>= processResponse

processPostRequest :: (ToJSON a, FromJSON b) => YnabConfig -> [T.Text] -> a -> IO (Either YnabError b)
processPostRequest config url body =
  formatUrl config url >>= formatPostRequest config body >>= processResponse

-- | All endpoints for YNAB's API
getUser :: YnabConfig -> IO (Either YnabError User)
getUser config = formatGetRequest config "GET https://api.youneedabudget.com/v1/user"
      >>= processResponse

getBudgets :: YnabConfig -> IO (Either YnabError BudgetSummaryResponse)
getBudgets config = processRequest (config { configBudgetId = Nothing }) ["GET"]

getBudgetById :: YnabConfig -> IO (Either YnabError BudgetDetailResponse)
getBudgetById config = processRequest config ["GET"]

getBudgetSettingsById :: YnabConfig -> IO (Either YnabError BudgetSettings)
getBudgetSettingsById config = processRequest config ["GET", "settings"]

getAccounts :: YnabConfig -> IO (Either YnabError AccountsSummaryResponse)
getAccounts config = processRequest config ["GET", "accounts"]

getAccountById :: YnabConfig -> AccountId -> IO (Either YnabError AccountDetailResponse)
getAccountById config aId = processRequest config ["GET", "accounts", aId]

getCategories :: YnabConfig -> IO (Either YnabError CategoriesResponse)
getCategories config = processRequest config ["GET", "categories"]

getCategoryById :: YnabConfig -> CategoryId -> IO (Either YnabError CategoryResponse)
getCategoryById config cId = processRequest config ["GET", "categories", cId]

-- getPayees - budgetId
getPayees :: YnabConfig -> IO (Either YnabError PayeesResponse)
getPayees config = processRequest config ["GET", "payees"]

-- getPayeeById - budgetId, payeeId
getPayeeById :: YnabConfig -> PayeeId -> IO (Either YnabError PayeeResponse)
getPayeeById config pId = processRequest config ["GET", "payees", pId]

-- getPayeeLocations - budgetId
getPayeeLocations :: YnabConfig -> IO (Either YnabError PayeeLocationsResponse)
getPayeeLocations config = processRequest config ["GET", "payee_locations"]

-- getPayeeLocationById - budgetId, payeeLocationId
getPayeeLocationById :: YnabConfig -> PayeeLocationId -> IO (Either YnabError PayeeLocationResponse)
getPayeeLocationById config plId= processRequest config ["GET", "payee_locations", plId]

-- getPayeeLocationByPayee - budgetId, payeeId
getPayeeLocationByPayee :: YnabConfig -> PayeeId -> IO (Either YnabError PayeeLocationResponse)
getPayeeLocationByPayee config pId = processRequest config ["GET", "payee_locations", pId]

-- getBudgetMonths - budgetId
getBudgetMonths :: YnabConfig -> IO (Either YnabError MonthSummariesResponse)
getBudgetMonths config = processRequest config ["GET", "months"]

-- getBudgetMonth - budgetId, Month
getBudgetMonth :: YnabConfig -> T.Text -> IO (Either YnabError MonthDetailResponse)
getBudgetMonth config month = processRequest config ["GET", "months", month]

-- getTransactions - budgetId
getTransactions :: YnabConfig -> IO (Either YnabError TransactionsResponse)
getTransactions config = processRequest config ["GET", "transactions"]

-- getTransactionsByAccount - budgetId accountId
getTransactionsByAccount :: YnabConfig -> AccountId -> IO (Either YnabError TransactionsResponse)
getTransactionsByAccount config aId = processRequest config ["GET", "accounts", aId, "transactions"]

-- getTransactionsByCategory - budgetId categoryId
getTransactionsByCategory :: YnabConfig  -> CategoryId -> IO (Either YnabError TransactionsResponse)
getTransactionsByCategory config cId = processRequest config ["GET", "categories", cId, "transactions"]

-- getTransactionsByPayee - budgetId payeeId
getTransactionsByPayee :: YnabConfig -> PayeeId -> IO (Either YnabError TransactionsResponse)
getTransactionsByPayee config pId = processRequest config ["GET", "payees", pId, "transactions"]

-- getTransactionById - budgetId transactionId
getTransactionById :: YnabConfig -> TransactionId -> IO (Either YnabError TransactionResponse)
getTransactionById config tId = processRequest config ["GET", "transactions", tId]

postTransaction :: YnabConfig -> SaveTransactionWrapper -> IO (Either YnabError SaveTransactionResponse)
postTransaction config = processPostRequest config ["POST", "transactions"]

postTransactions :: YnabConfig -> SaveTransactionsWrapper -> IO (Either YnabError SaveTransactionsResponse)
postTransactions config = processPostRequest config ["POST", "transactions"]

-- updateTransaction - budgetId transactionId Transaction
updateTransaction :: YnabConfig -> TransactionId -> SaveTransactionWrapper -> IO (Either YnabError TransactionResponse)
updateTransaction config tId = processPostRequest config ["PUT", "transactions", tId]

-- createTrasnaction - budgetId Transaction
-- bulkCreateTransactions - budgetId [Transaction]

-- getScheduledTransactions - budgetId
getScheduledTransactions :: YnabConfig -> IO (Either YnabError ScheduledTransactionsResponse)
getScheduledTransactions config = processRequest config ["GET", "scheduled_transactions"]

-- getScheduledTransactionById - budgetId, scheduleTransactionId
getScheduledTransactionById :: YnabConfig -> ScheduledTransactionId -> IO (Either YnabError ScheduledTransactionResponse)
getScheduledTransactionById config stId = processRequest config ["GET", "scheduled_transactions", stId]
