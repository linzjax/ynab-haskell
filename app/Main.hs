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
  , getCategories)
import Models.Budget (Budget(..), BudgetSummaryResponse(..))
import Models.Account (AccountsSummaryResponse(..), Account(..))
import Models.Category (CategoriesResponse(..), CategoryGroup(..))

main :: IO ()
main = do
  user <- getUser
  print "User: "
  print user
  getBudgets >>= \case
    Left err -> print err
    Right (BudgetSummaryResponse budgetList) -> do
      let bId = budgetId . head $ budgetList
      (getAccounts bId) >>= \case
         Left err -> print err
         Right (AccountsSummaryResponse accountList) -> do
           let aId = accountId . head $ accountList
           (getAccountById bId aId) >>= \case
              Left err -> print err
              Right a  -> print a
      (getCategories bId) >>= \case
          Left err -> print err
          Right (CategoriesResponse categoryGroupList) -> print categoryGroupList
