{-# LANGUAGE OverloadedStrings #-}
module Models.Account
  ( Account(..)
  , AccountsSummaryResponse(..)
  , AccountDetailResponse(..)
  ) where

import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

data AccountsSummaryResponse = AccountsSummaryResponse [Account] deriving Show

instance FromJSON AccountsSummaryResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    accountsObj <- respObj .: "accounts"
    return (AccountsSummaryResponse accountsObj)

data AccountDetailResponse = AccountDetailResponse Account deriving Show

instance FromJSON AccountDetailResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    accountObj <- respObj .: "account"
    return (AccountDetailResponse accountObj)

data Account = Account
  { accountId        :: !Text
  , name             :: !Text
  , accountType      :: !Text
  , onBudget         :: !Bool
  , closed           :: !Bool
  , note             :: !(Maybe Text)
  , balance          :: !Int
  , clearedBalance   :: !Int
  , unclearedBalance :: !Int
  , deleted          :: !Bool
  } deriving (Show)

instance FromJSON Account where
  parseJSON (Object o) = Account <$>
    o .: "id" <*>
    o .: "name" <*>
    o .: "type" <*>
    o .: "on_budget" <*>
    o .: "closed" <*>
    o .:? "note" <*>
    o .: "balance" <*>
    o .: "cleared_balance" <*>
    o .: "uncleared_balance" <*>
    o .: "deleted"
  parseJSON invalid = typeMismatch "Account" invalid
