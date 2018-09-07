{-# LANGUAGE OverloadedStrings #-}
module Models.User (User(..)) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as X
import Data.Text (Text)

newtype User = User { userId :: Text } deriving Show

instance FromJSON User where
  parseJSON (Object o) = do
    responseObject <- o .: "data"
    userObject     <- responseObject .: "user"
    userObjectId   <- userObject .: "id"
    return User{ userId = userObjectId }
  parseJSON invalid = typeMismatch "User" invalid
