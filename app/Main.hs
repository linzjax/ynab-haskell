{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import qualified Data.ByteString.Char8 as S8
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

import YNAB.Client
import YNAB.Models.Account
import YNAB.Models.Budget
import YNAB.Models.Category
import YNAB.Models.Config
import YNAB.Models.CurrencyFormat
import YNAB.Models.DateFormat
import YNAB.Models.Month
import YNAB.Models.Payee
import YNAB.Models.ScheduledTransaction
import YNAB.Models.Transaction
import YNAB.Models.User
import YNAB.Models.YnabError

main :: IO ()
main = do
  apiKey <- S8.pack . fromMaybe "" <$> lookupEnv "API_TOKEN"
  let config = YnabConfig apiKey Nothing
  user <- getUser config
  print "User: "
  print user
  -- | test GET /budgets
  getBudgets config >>= \case
    Left err -> print err
    Right (BudgetSummaryResponse budgetList) -> do
      print "successfully got /budgets"
      let bId = budgetId . head $ budgetList
      print "Budget: "
      print bId
      let config' = config { configBudgetId = Just bId }

      -- | test GET /accounts
      (getAccounts config') >>= \case
        Left err -> print err
        Right (AccountsSummaryResponse accountList) -> do
          print "successfully got /accounts"
          let aId = accountId . head $ accountList
           -- | test GET /accounts/<accountId>
          (getAccountById config' aId) >>= \case
              Left err -> print err
              Right (AccountDetailResponse _)  -> do
                print "successfully got /accounts/<aId>"

           -- | test GET /accounts/{account_id}/transactions
          (getTransactionsByAccount config' aId) >>= \case
              Left err -> print err
              Right (TransactionsResponse _) ->
                print "successfully got /accounts/{account_id}/transactions"

          -- | test POST /transactions
          let transaction = SaveTransaction aId "2018-09-29" (-000010) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          (postTransaction config' (SaveTransactionWrapper transaction)) >>= \case
              Left err -> print err
              Right (SaveTransactionResponse (SaveTransactionResponseWrapper [tId] _)) -> do
                print "successfully posted /accounts/{account_id}/transactions"
                let updatedT = SaveTransaction aId "2018-10-30" (-000020) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                (updateTransaction config tId (SaveTransactionWrapper updatedT)) >>= \case
                  Left err -> print err
                  Right (TransactionResponse r) -> do
                    if (transactionDate r == "2018-10-30" && transactionAmount r == -000020)
                      then print "successfully updated /accounts/{account_id}/transactions/{transaction_id}"
                      else print "this didn't actually update?"


          let transactions = [ (SaveTransaction aId "2018-09-29" (-000020) Nothing Nothing
                                Nothing (Just "delete me")  Nothing Nothing Nothing Nothing)
                             , (SaveTransaction aId "2018-09-29" (-000030) Nothing Nothing
                                Nothing (Just "delete me")  Nothing Nothing Nothing Nothing)
                             , (SaveTransaction aId "2018-09-29" (-000040) Nothing Nothing
                                Nothing (Just "delete me")  Nothing Nothing Nothing Nothing)]
          (postTransactions config' (SaveTransactionsWrapper transactions)) >>= \case
              Left err -> print err
              Right (SaveTransactionsResponse r) -> do
                print "successfully posted /accounts/{account_id}/transactions"

      -- | test GET /categories
      (getCategories config') >>= \case
          Left err -> print err
          Right (CategoriesResponse categoryGroupList) -> do
            print "Successfully got /categories"
            let cId = categoryId . head . cgCategories . head $ categoryGroupList
            -- | test GET /categories/<categoryId>
            (getCategoryById config' cId) >>= \case
                Left err -> print err
                Right (CategoryResponse _) ->
                  print "successfully got /categories/<cId>"

            -- | test GET /categories/{category_id}/transactions
            (getTransactionsByCategory config' cId) >>= \case
               Left err -> print err
               Right (TransactionsResponse _) ->
                 print "successfully got /categories/{category_id}/transactions"

      -- | test GET /payees
      (getPayees config') >>= \case
          Left err -> print err
          Right (PayeesResponse payeeList) -> do
            print "Successfully got /payees"
            let pId = payeeId . head $ payeeList

            -- | test GET /payees/<payeeID>
            (getPayeeById config' pId) >>= \case
                Left err -> print err
                Right (PayeeResponse _) -> do
                  print "successfully got /payees/<pId>"

                  -- | test GET /payee_locations
                  (getPayeeLocations config') >>= \case
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
            (getTransactionsByPayee config' pId) >>= \case
               Left err -> print err
               Right (TransactionsResponse _) -> do
                 print "successfully got /payees/{payee_id}/transactions"
      --
      -- | test GET /months
      (getBudgetMonths config') >>= \case
          Left err -> print err
          Right (MonthSummariesResponse _) -> do
            print "successfully got /months"

      -- | test GET /months/{month}
      (getBudgetMonth config' "current") >>= \case
          Left err -> print err
          Right (MonthDetailResponse _) -> do
            print "successfully got /months/current"

      -- | test GET /transactions
      (getTransactions config') >>= \case
          Left err -> print err
          Right (TransactionsResponse transactionList) -> do
            print "successfully got /transactions"
            let tId = transactionId . head $ transactionList

            -- | test GET /transactions/{transaction_id}
            (getTransactionById config' tId) >>= \case
                Left err -> print err
                Right (TransactionResponse _) ->
                  print "successfully got /transactions/{transaction_id}"

      -- | test GET /scheduled_transactions
      (getScheduledTransactions config') >>= \case
          Left err -> print err
          Right (ScheduledTransactionsResponse transactionList) -> do
            print "successfully got /scheduled_transactions"
            let stId = scheduledTransactionId . head $ transactionList

            -- | test GET /scheduled_transactions/{scheduled_transaction_id}
            (getScheduledTransactionById config' stId) >>= \case
                Left err -> print err
                Right (ScheduledTransactionResponse _) -> do
                  print "successfully got /scheduled_transactions/{scheduled_transaction_id}"
