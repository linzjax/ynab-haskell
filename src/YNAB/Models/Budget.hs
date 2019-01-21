{-# LANGUAGE OverloadedStrings #-}
module YNAB.Models.Budget
  ( Budget(..)
  , BudgetDetailResponse(..)
  , BudgetSummaryResponse(..)
  , BudgetSettings(..)
  , BudgetId
  ) where

import Data.Time (UTCTime)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

import YNAB.Models.Account (Account(..), AccountsSummaryResponse(..))
import YNAB.Models.Category (CategoryGroup(..), Category(..))
import YNAB.Models.CurrencyFormat (CurrencyFormat(..))
import YNAB.Models.DateFormat (DateFormat(..))
import YNAB.Models.Payee (Payee(..), PayeeLocation(..))
import YNAB.Models.Month (Month)
import YNAB.Models.Transaction (Transaction, Subtransaction)
import YNAB.Models.ScheduledTransaction (ScheduledTransaction, ScheduledSubtransaction)

-- | Model for storing the list of budgets retrieved by the /budgets endpoint
newtype BudgetSummaryResponse = BudgetSummaryResponse [Budget] deriving (Show)

instance FromJSON BudgetSummaryResponse where
  parseJSON (Object o) = do
    respObject    <- o .: "data"
    budgetObjects <- respObject .: "budgets"
    return $ BudgetSummaryResponse budgetObjects
  parseJSON invalid = typeMismatch "BudgetSummaryResponse" invalid

-- | Model for returning a single budget from the /budget/:id endpoint.
newtype BudgetDetailResponse = BudgetDetailResponse Budget deriving (Show)

instance FromJSON BudgetDetailResponse where
  parseJSON (Object o) = do
    respObject <- o .: "data"
    budgetObject <- respObject .: "budget"
    return $ BudgetDetailResponse budgetObject
  parseJSON invalid = typeMismatch "BudgetDetailResponse" invalid

-- | Individual Budget data objects
data Budget = Budget
  { budgetId             :: !BudgetId
  , budgetName           :: !Text
  , budgetCurrencyFormat :: !CurrencyFormat
  , budgetDateFormat     :: !DateFormat
  , budgetFirstMonth     :: !Text
  , budgetLastModifiedOn :: !UTCTime
  , budgetLastMonth      :: !Text

  -- | Not called until an individual budget is requested.
  , budgetAccounts                 :: !(Maybe [Account])
  , budgetPayees                   :: !(Maybe [Payee])
  , budgetPayeeLocations           :: !(Maybe [PayeeLocation])
  , budgetCategoryGroups           :: !(Maybe [CategoryGroup])
  , budgetCategories               :: !(Maybe [Category])
  , budgetMonths                   :: !(Maybe [Month])
  , budgetTransactions             :: !(Maybe [Transaction])
  , budgetSubtransactions          :: !(Maybe [Subtransaction])
  , budgetScheduledTransactions    :: !(Maybe [ScheduledTransaction])
  , budgetScheduledSubtransactions :: !(Maybe [ScheduledSubtransaction])
  } deriving (Show)

instance FromJSON Budget where
  parseJSON (Object o) = Budget <$>
    o .: "id" <*>
    o .: "name" <*>
    o .: "currency_format" <*>
    o .: "date_format" <*>
    o .: "first_month" <*>
    o .: "last_modified_on" <*>
    o .: "last_month" <*>

    o .:? "accounts" <*>
    o .:? "payees" <*>
    o .:? "payee_locations" <*>
    o .:? "category_groups" <*>
    o .:? "categories" <*>
    o .:? "months" <*>
    o .:? "transactions" <*>
    o .:? "subtransactions" <*>
    o .:? "scheduled_transactions" <*>
    o .:? "scheduled_subtransactions"
  parseJSON invalid = typeMismatch "Budget" invalid


data BudgetSettings = BudgetSettings
  { bSettingsCurrencyFormat :: !CurrencyFormat
  , bSettingsDateFormat     :: !DateFormat
  } deriving Show

instance FromJSON BudgetSettings where
  parseJSON (Object o) = do
    respObject <- o .: "data"
    settingsObject <- respObject .: "settings"
    sCurrencyFormat <- settingsObject .: "currency_format"
    sDateFormat <- settingsObject .: "date_format"
    return BudgetSettings { bSettingsCurrencyFormat = sCurrencyFormat
                          , bSettingsDateFormat = sDateFormat }
  parseJSON invalid = typeMismatch "BudgetSettings" invalid

type BudgetId = Text


--
-- --
-- newtype AccountList = AccountList [Account] deriving Show
--
-- instance FromJSON AccountList where
--   parseJSON (Object o) = AccountList <$>
--     o .: "accounts"
--   parseJSON invalid = typeMismatch "CategoryGroupList" invalid
--
-- newtype PayeeList = PayeeList [Payee] deriving Show
--
-- instance FromJSON PayeeList where
--   parseJSON (Object o) = PayeeList <$>
--     o .: "payees"
--   parseJSON invalid = typeMismatch "CategoryGroupList" invalid
--
-- newtype PayeeLocationList = PayeeLocationList [PayeeLocation] deriving Show
--
-- instance FromJSON PayeeLocationList where
--   parseJSON (Object o) = PayeeLocationList <$>
--     o .: "payee_locations"
--   parseJSON invalid = typeMismatch "CategoryGroupList" invalid
--
-- -- | Category type for parsing individual budget calls.
-- newtype CategoryList = CategoryList [Category] deriving Show
--
-- instance FromJSON CategoryList where
--   parseJSON (Object o) = CategoryList <$>
--     o .: "categories"
--   parseJSON invalid = typeMismatch "CategoryList" invalid
--
-- -- | Category type for parsing individual budget calls.
-- newtype CategoryGroupList = CategoryGroupList [CategoryGroup] deriving Show
--
-- instance FromJSON CategoryGroupList where
--   parseJSON (Object o) = CategoryGroupList <$>
--     o .: "category_groups"
--   parseJSON invalid = typeMismatch "CategoryGroupList" invalid
--
-- -- | Category type for parsing individual budget calls.
-- newtype MonthList = MonthList [Month] deriving Show
--
-- instance FromJSON MonthList where
--   parseJSON (Object o) = MonthList <$>
--     o .: "months"
--   parseJSON invalid = typeMismatch "MonthList" invalid
--
-- -- | Category type for parsing individual budget calls.
-- newtype TransactionList = TransactionList [Transaction] deriving Show
--
-- instance FromJSON TransactionList where
--   parseJSON (Object o) = TransactionList <$>
--     o .: "transactions"
--   parseJSON invalid = typeMismatch "TransactionList" invalid
--
-- -- | Category type for parsing individual budget calls.
-- newtype SubtransactionList = SubtransactionList [Subtransaction] deriving Show
--
-- instance FromJSON SubtransactionList where
--   parseJSON (Object o) = SubtransactionList <$>
--     o .: "subtransactions"
--   parseJSON invalid = typeMismatch "SubtransactionList" invalid
--
-- --
-- -- | Category type for parsing individual budget calls.
-- newtype TransactionList = TransactionList [Transaction] deriving Show
--
-- instance FromJSON TransactionList where
--   parseJSON (Object o) = TransactionList <$>
--     o .: "transactions"
--   parseJSON invalid = typeMismatch "TransactionList" invalid
--
-- -- | Category type for parsing individual budget calls.
-- newtype SubtransactionList = SubtransactionList [Subtransaction] deriving Show
--
-- instance FromJSON SubtransactionList where
--   parseJSON (Object o) = SubtransactionList <$>
--     o .: "subtransactions"
--   parseJSON invalid = typeMismatch "SubtransactionList" invalid

--
--
