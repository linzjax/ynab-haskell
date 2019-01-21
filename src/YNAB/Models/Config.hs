module YNAB.Models.Config (YnabConfig(..)) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.Text as T

data YnabConfig = YnabConfig
  { getKey   :: ByteString
  , configBudgetId :: Maybe T.Text
  } deriving (Show)

-- apiKey <- S8.pack . fromMaybe "" <$> lookupEnv "API_TOKEN"
