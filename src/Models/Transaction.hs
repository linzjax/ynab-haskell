{-# LANGUAGE OverloadedStrings #-}
module Models.Transaction
  ( Transaction(..)
  , TransactionId
  , TransactionsResponse(..)
  , TransactionResponse(..)
  , Subtransaction(..)
  , SaveTransaction(..)
  , SaveTransactionResponse(..)
  ) where

--
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , object
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

import Models.Payee (PayeeId)
import Models.Account (AccountId)
import Models.Category (CategoryId)

data TransactionsResponse = TransactionsResponse [Transaction] deriving Show

instance FromJSON TransactionsResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    transactionsObj <- respObj .: "transactions"
    return (TransactionsResponse transactionsObj)
  parseJSON invalid = typeMismatch "TransactionsResponse" invalid

data TransactionResponse = TransactionResponse Transaction deriving Show

instance FromJSON TransactionResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    transactionObj <- respObj .: "transaction"
    return (TransactionResponse transactionObj)
  parseJSON invalid = typeMismatch "TransactionResponse" invalid

type TransactionId = Text

data Transaction = Transaction
  { transactionId                :: !TransactionId
  , transactionDate              :: !Text -- | TODO: This should be a date format
  , transactionAmount            :: !Int
  , transactionMemo              :: !(Maybe Text)
  , transactionCleared           :: !Text -- | TODO: Does this have specific responses?
  , transactionApproved          :: !Bool
  , transactionFlagColor         :: !(Maybe Text) -- | TODO: Does this have specific responses?
  , transactionAccountId         :: !AccountId -- | TODO: This should refer to an Account obj.
  , transactionPayeeId           :: !(Maybe PayeeId) -- | TODO: This should refer to an Payee obj.
  , transactionCategoryId        :: !(Maybe CategoryId) -- | TODO: This should refer to an Category obj.
  , transactionTransferAccountId :: !(Maybe AccountId) -- | TODO: This should be an accont obj?
  , transactionImportId          :: !(Maybe Text)
  , transactionDeleted           :: !Bool
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

-- TODO: Figure out how to convert string options "cleared", "uncleared", "reconciled" to ClearedTransaction
-- data ClearedTransaction = Cleared | Uncleared | Reconciled deriving (Show, Read)

data Subtransaction = Subtransaction
  { subtId                :: !Text
  , subtTransactionId     :: !TransactionId -- | TODO: This should connect to a transaction.
  , subtAmount            :: !Int
  , subtMemo              :: !(Maybe Text)
  , subtPayeeId           :: !(Maybe PayeeId) -- | TODO: This should connect to a payee
  , subtCategoryId        :: !(Maybe CategoryId) -- | TODO: This should connect to a category
  , subtTransferAccountId :: !(Maybe AccountId) -- | TODO: This should connect to an Account
  , subtDeleted           :: !Bool
  } deriving (Show)

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

data SaveTransactionResponse = SaveTransactionResponse SaveTransactionWrapper deriving Show

instance FromJSON SaveTransactionResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    return (SaveTransactionResponse respObj)
  parseJSON invalid = typeMismatch "SaveTransactionResponse" invalid

data SaveTransactionWrapper = SaveTransactionWrapper [TransactionId] Transaction deriving Show

instance FromJSON SaveTransactionWrapper where
  parseJSON (Object o) = SaveTransactionWrapper <$>
    o .: "transaction_ids" <*>
    o .: "transaction"
  parseJSON invalid = typeMismatch "SaveTransactionWrapper" invalid

data SaveTransaction = SaveTransaction
  { saveAccountId   :: !AccountId
  , saveDate        :: !Text
  , saveAmount      :: !Int
  -- , savePayeeId     :: !(Maybe PayeeId)
  -- , savePayeeName   :: !(Maybe Text)
  -- , saveCategoryId  :: !(Maybe CategoryId)
  -- , saveMemo        :: !(Maybe Text)
  -- , saveCleared     :: !(Maybe Text) -- [cleared, uncleared, reconciled]
  -- , savedApproved   :: !(Maybe Bool)
  -- , saveFlagColor   :: !(Maybe Text) -- [red, orange, yellow, green, blue, purple]
  -- , saveImportId    :: !(Maybe Text) -- 'YNAB:-294230:2015-12-30:1’
  }

instance ToJSON SaveTransaction where
  toJSON (SaveTransaction saId sDate sAmount) = object
    ["transaction" .= object
      [ "account_id" .= saId
      , "date" .= sDate
      , "amount" .= sAmount
      ]
    ]
