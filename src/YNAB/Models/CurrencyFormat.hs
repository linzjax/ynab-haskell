{-# LANGUAGE OverloadedStrings #-}
module YNAB.Models.CurrencyFormat ( CurrencyFormat(..)) where

--
import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

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
