{-# LANGUAGE OverloadedStrings #-}
module Models.Transaction
  ( Transaction(..)
  , TransactionId
  , TransactionsResponse(..)
  , TransactionResponse(..)
  , Subtransaction(..)
  ) where

--
import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

data TransactionsResponse = TransactionsResponse [Transaction] deriving Show

instance FromJSON TransactionsResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    transactionsObj <- respObj .: "transactions"
    return (TransactionsResponse transactionsObj)

data TransactionResponse = TransactionResponse Transaction deriving Show

instance FromJSON TransactionResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    transactionObj <- respObj .: "transaction"
    return (TransactionResponse transactionObj)

type TransactionId = Text

data Transaction = Transaction
  { transactionId                :: !TransactionId
  , transactionDate              :: !Text -- | TODO: This should be a date format
  , transactionAmount            :: !Int
  , transactionMemo              :: !(Maybe Text)
  , transactionCleared           :: !Text -- | TODO: Does this have specific responses?
  , transactionApproved          :: !Bool
  , transactionFlagColor         :: !(Maybe Text) -- | TODO: Does this have specific responses?
  , transactionAccountId         :: !Text -- | TODO: This should refer to an Account obj.
  , transactionPayeeId           :: !(Maybe Text) -- | TODO: This should refer to an Payee obj.
  , transactionCategoryId        :: !(Maybe Text) -- | TODO: This should refer to an Category obj.
  , transactionTransferAccountId :: !(Maybe Text) -- | TODO: This should be an accont obj?
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
