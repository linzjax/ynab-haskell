{-# LANGUAGE OverloadedStrings #-}
module Models.Payee
  ( PayeesResponse(..)
  , PayeeResponse(..)
  , Payee(..)
  , PayeeLocation(..)
  ) where

import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

data PayeesResponse = PayeesResponse [Payee] deriving Show

instance FromJSON PayeesResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    payeesObj <- respObj .: "payees"
    return $ PayeesResponse payeesObj
  parseJSON invalid = typeMismatch "PayeesResponse" invalid

data PayeeResponse = PayeeResponse Payee deriving Show

instance FromJSON PayeeResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    payeeObj <- respObj .: "payee"
    return $ PayeeResponse payeeObj
  parseJSON invalid = typeMismatch "PayeeResponse" invalid

data Payee = Payee
  { payeeId                :: !Text
  , payeeName              :: !Text
  , payeeTransferAccountId :: !(Maybe Text)
  , payeeDeleted           :: !Bool
  } deriving (Show)

instance FromJSON Payee where
  parseJSON (Object o) = Payee <$>
    o .: "id" <*>
    o .: "name" <*>
    o .:? "transfer_account_id" <*>
    o .: "deleted"
  parseJSON invalid = typeMismatch "Payee" invalid

data PayeeLocation = PayeeLocation
  { plId        :: !Text
  , plPayeeId   :: !Text -- | TODO: This should be a reference to a Payee.
  , plLatitude  :: !(Maybe Text)
  , plLongitude :: !(Maybe Text)
  , plDeleted   :: !Bool
  } deriving (Show)
--
instance FromJSON PayeeLocation where
  parseJSON (Object o) = PayeeLocation <$>
    o .: "id" <*>
    o .: "payee_id" <*>
    o .:? "latitude" <*>
    o .:? "logitude" <*>
    o .: "deleted"
  parseJSON invalid = typeMismatch "PayeeLocation" invalid
