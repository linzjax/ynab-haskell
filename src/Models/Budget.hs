{-# LANGUAGE OverloadedStrings #-}
module Models.Budget (Budget(..), BudgetList(..)) where

import Data.Time (UTCTime)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as X
import Data.Text (Text)

-- | Model for storing the list of budgets retrieved by the /budgets endpoint
newtype BudgetList = BudgetList [Budget] deriving (Show)

instance FromJSON BudgetList where
  parseJSON (Object o) = do
    respObject    <- o .: "data"
    budgetObjects <- respObject .: "budgets"
    return $ BudgetList budgetObjects
  parseJSON invalid = typeMismatch "BudgetList" invalid

-- | Individual Budget data objects
data Budget = Budget
  { budgetId :: !Text
  , budgetName :: !Text
  , budgetCurrencyFormat :: CurrencyFormat
  , budgetDateFormat :: DateFormat
  , budgetFirstMonth :: !Text
  , budgetLastModifiedOn :: !UTCTime
  , budgetLastMonth :: !Text
  } deriving (Show)

instance FromJSON Budget where
  parseJSON (Object o) = Budget <$>
    o .: "id" <*>
    o .: "name" <*>
    o .: "currency_format" <*>
    o .: "date_format" <*>
    o .: "first_month" <*>
    o .: "last_modified_on" <*>
    o .: "last_month"
  parseJSON invalid = typeMismatch "Budget" invalid

-- | Object for parsing out the currency format
data CurrencyFormat = CurrencyFormat
  { cfCurrencySymbol   :: !Text
  , cfDecimalDigits    :: !Int
  , cfDecimalSeparator :: !Text
  , cfDisplaySymbol    :: !Bool
  , cfExampleFormat    :: !Text
  , cfGroupSeparator   :: !Text
  , cfISOCode          :: !Text
  , cfSymbolFirst      :: !Bool
  } deriving (Show)

instance FromJSON CurrencyFormat where
  parseJSON (Object o) = CurrencyFormat <$>
    o .: "currency_symbol" <*>
    o .: "decimal_digits" <*>
    o .: "decimal_separator" <*>
    o .: "display_symbol" <*>
    o .: "example_format" <*>
    o .: "group_separator" <*>
    o .: "iso_code" <*>
    o .: "symbol_first"
  parseJSON invalid = typeMismatch "CurrencyFormat" invalid

-- | Object for storing the date format.
newtype DateFormat = DateFormat { dfFormat :: Text } deriving (Show)

instance FromJSON DateFormat where
  parseJSON (Object o) = DateFormat <$> o .: "format"
