{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Network.HTTP.Simple
import System.Environment (lookupEnv)

import Client
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
  , getScheduledTransactions
  , getScheduledTransactionById
  )
import Models.Budget (Budget(..), BudgetSummaryResponse(..))
import Models.Account (AccountsSummaryResponse(..), AccountDetailResponse(..), Account(..))
import Models.Category ( CategoriesResponse(..), CategoryGroup(..)
                       , CategoryResponse(..), Category(..))
import Models.Payee
  ( PayeesResponse(..)
  , PayeeResponse(..)
  , Payee(..)
  , PayeeLocationsResponse(..)
  , PayeeLocationResponse(..)
  , PayeeLocation(..))
import Models.Month
  ( Month(..)
  , MonthSummariesResponse(..)
  , MonthDetailResponse(..))
import Models.Transaction
  ( Transaction(..)
  , TransactionsResponse(..)
  , TransactionResponse(..)
  )
import Models.ScheduledTransaction
  ( ScheduledTransaction(..)
  , ScheduledTransactionsResponse(..)
  , ScheduledTransactionResponse(..))

main :: IO ()
main = do
  user <- getUser
  print "User: "
  print user
  -- | test GET /budgets
  getBudgets >>= \case
    Left err -> print err
    Right (BudgetSummaryResponse budgetList) -> do
      print "successfully got /budgets"
      let bId = budgetId . head $ budgetList

      -- | test GET /accounts
      (getAccounts bId) >>= \case
         Left err -> print err
         Right (AccountsSummaryResponse accountList) -> do
           print "successfully got /accounts"
           let aId = accountId . head $ accountList
           -- | test GET /accounts/<accountId>
           (getAccountById bId aId) >>= \case
              Left err -> print err
              Right (AccountDetailResponse _)  -> do
                print "successfully got /accounts/<aId>"

           -- | test GET /accounts/{account_id}/transactions
           (getTransactionsByAccount bId aId) >>= \case
              Left err -> print err
              Right (TransactionsResponse _) ->
                print "successfully got /accounts/{account_id}/transactions"


      -- | test GET /categories
      (getCategories bId) >>= \case
          Left err -> print err
          Right (CategoriesResponse categoryGroupList) -> do
            print "Successfully got /categories"
            let cId = categoryId . head . cgCategories . head $ categoryGroupList
            -- | test GET /categories/<categoryId>
            (getCategoryById bId cId) >>= \case
                Left err -> print err
                Right (CategoryResponse _) ->
                  print "successfully got /categories/<cId>"

            -- | test GET /categories/{category_id}/transactions
            (getTransactionsByCategory bId cId) >>= \case
               Left err -> print err
               Right (TransactionsResponse _) ->
                 print "successfully got /categories/{category_id}/transactions"

      -- | test GET /payees
      (getPayees bId) >>= \case
          Left err -> print err
          Right (PayeesResponse payeeList) -> do
            print "Successfully got /payees"
            let pId = payeeId . head $ payeeList

            -- | test GET /payees/<payeeID>
            (getPayeeById bId pId) >>= \case
                Left err -> print err
                Right (PayeeResponse _) -> do
                  print "successfully got /payees/<pId>"

                  -- | test GET /payee_locations
                  (getPayeeLocations bId) >>= \case
                      Left err -> print err
                      Right (PayeeLocationsResponse payeeLocationList) -> do
                        print "successfully got /payee_locations"
                        -- | test GET /payee_locations/<payeeLocationId>
                        -- So the problem is I don't use this feature, so I can't test it...
                        -- let plocId = plId . head $ payeeLocationList
                        -- (getPayeeLocationById bId plocId) >>= \case
                        --     Left err -> print err
                        --     Right (PayeeLocationResponse payeeLocation) -> print payeeLocation

                  -- | test GET /payee_locations/<payeeId>
                  -- | Same problem here...
                  -- (getPayeeLocationById bId pId) >>= \case
                  --     Left err -> print err
                  --     Right (PayeeLocationResponse payeeLocation) -> print payeeLocation

            -- | test GET /payees/{payee_id}/transactions
            (getTransactionsByPayee bId pId) >>= \case
               Left err -> print err
               Right (TransactionsResponse _) -> do
                 print "successfully got /payees/{payee_id}/transactions"
      --
      -- | test GET /months
      (getBudgetMonths bId) >>= \case
          Left err -> print err
          Right (MonthSummariesResponse _) -> do
            print "successfully got /months"

      -- | test GET /months/{month}
      (getBudgetMonth bId "current") >>= \case
          Left err -> print err
          Right (MonthDetailResponse _) -> do
            print "successfully got /months/current"

      -- | test GET /transactions
      (getTransactions bId) >>= \case
          Left err -> print err
          Right (TransactionsResponse transactionList) -> do
            print "successfully got /transactions"
            let tId = transactionId . head $ transactionList

            -- | test GET /transactions/{transaction_id}
            (getTransactionById bId tId) >>= \case
                Left err -> print err
                Right (TransactionResponse _) ->
                  print "successfully got /transactions/{transaction_id}"

      -- | test GET /scheduled_transactions
      (getScheduledTransactions bId) >>= \case
          Left err -> print err
          Right (ScheduledTransactionsResponse transactionList) -> do
            print "successfully got /scheduled_transactions"
            let stId = scheduledTransactionId . head $ transactionList

            -- | test GET /scheduled_transactions/{scheduled_transaction_id}
            (getScheduledTransactionById bId stId) >>= \case
                Left err -> print err
                Right (ScheduledTransactionResponse _) -> do
                  print "successfully got /scheduled_transactions/{scheduled_transaction_id}"
