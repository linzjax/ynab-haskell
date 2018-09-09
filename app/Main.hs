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
  )
import Models.Budget (Budget(..), BudgetSummaryResponse(..))
import Models.Account (AccountsSummaryResponse(..), Account(..))
import Models.Category (CategoriesResponse(..), CategoryGroup(..), CategoryResponse(..), Category(..))

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
