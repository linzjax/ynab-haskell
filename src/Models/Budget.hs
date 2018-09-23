{-# LANGUAGE OverloadedStrings #-}
module Models.Budget
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

import Models.Account (Account(..))
import Models.Category (CategoryGroup(..), Category(..))
import Models.Payee (Payee(..), PayeeLocation(..))

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
  { budgetId       :: !BudgetId
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
-- | TODO: actually use this to format dates correctly
-- | parseTimeM True defaultTimeLocale "%-m/%-d/%Y" "4/30/2018" :: Maybe Day
newtype DateFormat = DateFormat { dfFormat :: Text } deriving (Show)

instance FromJSON DateFormat where
  parseJSON (Object o) = DateFormat <$> o .: "format"
  parseJSON invalid = typeMismatch "DateFormat" invalid

data Month = Month
  { monthName :: !String -- | TODO: This should be a date format.
  , monthNote :: !(Maybe String)
  , monthIncome :: !(Maybe Int)
  , monthBudgeted :: !(Maybe Int)
  , monthActivity :: !(Maybe Int)
  , monthToBeBudgeted :: !(Maybe Int)
  , monthAgeOfMoney :: !(Maybe Int)
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


data Transaction = Transaction
  { tranId                :: !Text
  , tranDate              :: !Text -- | TODO: This should be a date format
  , tranAmount            :: !Int
  , tranMemo              :: !(Maybe Text)
  , tranCleared           :: !Text -- | TODO: Does this have specific responses?
  , tranApproved          :: !Bool
  , tranFlagColor         :: !(Maybe Text) -- | TODO: Does this have specific responses?
  , tranAccountId         :: !Text -- | TODO: This should refer to an Account obj.
  , tranPayeeId           :: !(Maybe Text) -- | TODO: This should refer to an Payee obj.
  , tranCategoryId        :: !(Maybe Text) -- | TODO: This should refer to an Category obj.
  , tranTransferAccountId :: !(Maybe Text) -- | TODO: This should be an accont obj?
  , tranImportId          :: !(Maybe Text)
  , tranDeleted           :: !Bool
  } deriving (Show)

instance FromJSON Transaction where
  parseJSON (Object o) = Transaction <$>
    o .: "id" <*>
    o .: "date" <*>
    o .: "amount" <*>
    o .:? "memo" <*>
    o .: "cleared" <*>
    o .: "approved" <*>
    o .:? "flag_color" <*>
    o .: "account_id" <*>
    o .:? "payee_id" <*>
    o .:? "category_id" <*>
    o .:? "transfer_account_id" <*>
    o .:? "import_id" <*>
    o .: "deleted"
  parseJSON invalid = typeMismatch "Transaction" invalid

data Subtransaction = Subtransaction
  { subtId                :: !Text
  , subtTransactionId     :: !Text -- | TODO: This should connect to a transaction.
  , subtAmount            :: !Int
  , subtMemo              :: !(Maybe Text)
  , subtPayeeId           :: !(Maybe Text) -- | TODO: This should connect to a payee
  , subtCategoryId        :: !(Maybe Text) -- | TODO: This should connect to a category
  , subtTransferAccountId :: !(Maybe String) -- | TODO: This should connect to an Account
  , subtDeleted           :: !Bool
  } deriving (Show)
--
instance FromJSON Subtransaction where
  parseJSON (Object o) = Subtransaction <$>
    o .: "id" <*>
    o .: "transaction_id" <*>
    o .: "amount" <*>
    o .:? "memo" <*>
    o .:? "payee_id" <*>
    o .:? "category_id" <*>
    o .:? "transfer_account_id" <*>
    o .: "deleted"
  parseJSON invalid = typeMismatch "Subtransaction" invalid

data ScheduledTransaction = ScheduledTransaction
  { stId                :: !Text
  , stDateFirst         :: !Text
  , stDateNext          :: !Text
  , stFrequency         :: !Text -- | TODO: Does this have specific responses?
  , stAmount            :: !Int
  , stMemo              :: !(Maybe Text)
  , stFlagColor         :: !(Maybe Text) -- | TODO: Does this have specific responses?
  , stAccountId         :: !Text -- | TODO: This should connect to an Account
  , stPayeeId           :: !(Maybe Text) -- | TODO: This should connect to an Account
  , stCategoryId        :: !(Maybe Text)
  , stTransferAccountId :: !(Maybe Text)
  , stDeleted           :: !Bool
  } deriving (Show)
--
instance FromJSON ScheduledTransaction where
  parseJSON (Object o) = ScheduledTransaction <$>
    o .: "id" <*>
    o .: "date_first" <*>
    o .: "date_next" <*>
    o .: "frequency" <*>
    o .: "amount" <*>
    o .:? "memo" <*>
    o .:? "flag_color" <*>
    o .: "account_id" <*>
    o .:? "payee_id" <*>
    o .:? "category_id" <*>
    o .:? "transfer_account_id" <*>
    o .: "deleted"
  parseJSON invalid = typeMismatch "ScheduledTransaction" invalid

data ScheduledSubtransaction = ScheduledSubtransaction
  { sSubtId                     :: !Text
  , sSubtScheduledTransactionId :: !Text
  , sSubtAmount                 :: !Int
  , sSubtMemo                   :: !(Maybe Text)
  , sSubtPayeeId                :: !(Maybe Text)
  , sSubtCategoryId             :: !(Maybe Text)
  , sSubtTransferAccountId      :: !(Maybe Text)
  , sSubtDeleted                :: !Bool
  } deriving (Show)
--
instance FromJSON ScheduledSubtransaction where
  parseJSON (Object o) = ScheduledSubtransaction <$>
    o .: "id" <*>
    o .: "scheduled_transaction_id" <*>
    o .: "amount" <*>
    o .:? "memo" <*>
    o .:? "payee_id" <*>
    o .:? "category_id" <*>
    o .:? "transfer_account_id" <*>
    o .: "deleted"
  parseJSON invalid = typeMismatch "ScheduledSubtransaction" invalid
