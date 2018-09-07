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

import Client (getUser, getBudgets, getBudget)
import Models.Budget (Budget(..), BudgetSummaryResponse(..))

main :: IO ()
main = do
  user <- getUser
  print "User: "
  print user
  getBudgets >>= \case
    Nothing -> print "boooo"
    Just (BudgetSummaryResponse budgetList) -> do
      print "BudgetList: "
      print budgetList
      -- print budgetList
      let budget = head budgetList
      (getBudget $ budgetId budget) >>= \case
         Just b -> print b
         Nothing -> print "booo"
