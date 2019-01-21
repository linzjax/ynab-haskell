{-# LANGUAGE OverloadedStrings #-}
module YNAB.Models.Month
  ( Month(..)
  , MonthSummariesResponse(..)
  , MonthDetailResponse(..)
  ) where

--
import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

import YNAB.Models.Category (Category)

data MonthSummariesResponse = MonthSummariesResponse [Month] deriving Show

instance FromJSON MonthSummariesResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    monthsObj <- respObj .: "months"
    return (MonthSummariesResponse monthsObj)

data MonthDetailResponse = MonthDetailResponse Month deriving Show

instance FromJSON MonthDetailResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    monthObj <- respObj .: "month"
    return (MonthDetailResponse monthObj)

data Month = Month
  { monthName :: !String -- | TODO: This should be a date format.
  , monthNote :: !(Maybe String)
  , monthIncome :: !(Maybe Int)
  , monthBudgeted :: !(Maybe Int)
  , monthActivity :: !(Maybe Int)
  , monthToBeBudgeted :: !(Maybe Int)
  , monthAgeOfMoney :: !(Maybe Int)

  -- Only returned in MonthDetailResponse
  , monthCategories :: !(Maybe [Category])
  } deriving (Show)
--
instance FromJSON Month where
  parseJSON (Object o) = Month <$>
    o .: "month" <*>
    o .:? "note" <*>
    o .:? "income" <*>
    o .:? "budgeted" <*>
    o .:? "activity" <*>
    o .:? "to_be_budgeted" <*>
    o .:? "age_of_money" <*>
    o .:? "categories"
  parseJSON invalid = typeMismatch "Month" invalid
