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
  )
import Models.Budget (Budget(..), BudgetSummaryResponse(..))
import Models.Account (AccountsSummaryResponse(..), Account(..))
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
              Right a  -> print "successfully got /accounts/<aId>"

      -- | test GET /categories
      (getCategories bId) >>= \case
          Left err -> print err
          Right (CategoriesResponse categoryGroupList) -> do
            print "Successfully got /categories"
            let cId = categoryId . head . cgCategories . head $ categoryGroupList
            -- | test GET /categories/<categoryId>
            (getCategoryById bId cId) >>= \case
                Left err -> print err
                Right (CategoryResponse category) -> print "sucessfully got /categories/<cId>"

      -- | test GET /payees
      (getPayees bId) >>= \case
          Left err -> print err
          Right (PayeesResponse payeeList) -> do
            print "Successfully got /payees"
            let pId = payeeId . head $ payeeList

            -- | test GET /payees/<payeeID>
            (getPayeeById bId pId) >>= \case
                Left err -> print err
                Right (PayeeResponse payee) -> do
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
      --
      -- | test GET /months
      (getBudgetMonths bId) >>= \case
          Left err -> print err
          Right (MonthSummariesResponse months) -> do
            print "successfully get /months"

      -- | test GET /months/{month}
      (getBudgetMonth bId "current") >>= \case
          Left err -> print err
          Right (MonthDetailResponse month) -> do
            print "successfully get /months/current"
