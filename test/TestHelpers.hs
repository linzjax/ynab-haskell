module TestHelpers where

import qualified Data.ByteString.Char8 as S8
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

import YNAB.Client
import YNAB.Models.Budget
import YNAB.Models.Config

withFirstBudget :: YnabConfig -> IO Budget
withFirstBudget config =
  getBudgets config >>= \case
    Right (BudgetSummaryResponse (b:bs)) ->
      return b

setConfig :: IO YnabConfig
setConfig = do
  apiKey <- S8.pack . fromMaybe "" <$> lookupEnv "API_TOKEN"
  b <- withFirstBudget (YnabConfig apiKey Nothing)
  return YnabConfig { getKey = apiKey
                    , configBudgetId = Just (budgetId b) }
