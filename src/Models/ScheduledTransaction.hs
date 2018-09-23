{-# LANGUAGE OverloadedStrings #-}
module Models.ScheduledTransaction
  ( ScheduledTransaction(..)
  , ScheduledTransactionId
  , ScheduledTransactionsResponse(..)
  , ScheduledTransactionResponse(..)
  , ScheduledSubtransaction(..)
  ) where

--
import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

data ScheduledTransactionsResponse = ScheduledTransactionsResponse [ScheduledTransaction] deriving Show

instance FromJSON ScheduledTransactionsResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    transactionsObj <- respObj .: "scheduled_transactions"
    return (ScheduledTransactionsResponse transactionsObj)

data ScheduledTransactionResponse = ScheduledTransactionResponse ScheduledTransaction deriving Show

instance FromJSON ScheduledTransactionResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    transactionObj <- respObj .: "scheduled_transaction"
    return (ScheduledTransactionResponse transactionObj)

type ScheduledTransactionId = Text


data ScheduledTransaction = ScheduledTransaction
  { scheduledTransactionId                :: !Text
  , scheduledTransactionDateFirst         :: !Text
  , scheduledTransactionDateNext          :: !Text
  , scheduledTransactionFrequency         :: !Text -- | TODO: Does this have specific responses?
  , scheduledTransactionAmount            :: !Int
  , scheduledTransactionMemo              :: !(Maybe Text)
  , scheduledTransactionFlagColor         :: !(Maybe Text) -- | TODO: Does this have specific responses?
  , scheduledTransactionAccountId         :: !Text -- | TODO: This should connect to an Account
  , scheduledTransactionPayeeId           :: !(Maybe Text) -- | TODO: This should connect to an Account
  , scheduledTransactionCategoryId        :: !(Maybe Text)
  , scheduledTransactionTransferAccountId :: !(Maybe Text)
  , scheduledTransactionDeleted           :: !Bool
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
  { scheduledSubtId                     :: !Text
  , scheduledSubtScheduledTransactionId :: !Text
  , scheduledSubtAmount                 :: !Int
  , scheduledSubtMemo                   :: !(Maybe Text)
  , scheduledSubtPayeeId                :: !(Maybe Text)
  , scheduledSubtCategoryId             :: !(Maybe Text)
  , scheduledSubtTransferAccountId      :: !(Maybe Text)
  , scheduledSubtDeleted                :: !Bool
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
