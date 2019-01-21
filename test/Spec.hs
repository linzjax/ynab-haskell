{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import Control.Monad.IO.Class (liftIO)
import Control.Exception (evaluate)
import Data.Either
import Test.Hspec
import Test.QuickCheck

import YNAB.Client
import YNAB.Models.Account
import YNAB.Models.Budget
import YNAB.Models.Config

import TestHelpers

main :: IO ()
main = do
  config <- setConfig
  hspec $ do
    describe "Budget tests" $ do
      it "Gets a list of budgets" $ do
        result <- getBudgets config
        result `shouldSatisfy` isRight

      it "Gets a single budget" $
        getBudgets config >>= \case
          Right (BudgetSummaryResponse (b:bs)) -> do
            b' <- getBudgetById config
            b' `shouldSatisfy` isRight
            -- bad <- getBudgetById $ config { configBudgetId = Just "not_an_id" }
            -- bad `shouldSatisfy` isLeft

    describe "Account tests" $ do
      it "Gets a list of accounts" $ do
        result <- getAccounts config
        result `shouldSatisfy` isRight

      it "Gets a single account" $
        getAccounts config >>= \case
          Right (AccountsSummaryResponse (a:_)) -> do
            a' <- getAccountById config $ accountId a
            a' `shouldSatisfy` isRight

    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)
    --
    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException
