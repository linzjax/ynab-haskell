{-# LANGUAGE OverloadedStrings #-}
module YNAB.Models.YnabError (YnabError(..)) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as X
import Data.Text (Text)

data YnabError = YnabError
  { errorId     :: !Text
  , errorName   :: !Text
  , errorDetail :: !Text } deriving Show

instance FromJSON YnabError where
  parseJSON (Object o) = do
    respObject <- o .: "error"
    errId     <- respObject .: "id"
    errName   <- respObject .: "name"
    errDetail <- respObject .: "detail"
    return YnabError { errorId = errId
                     , errorName = errName
                     , errorDetail = errDetail
                     }
  parseJSON invalid = typeMismatch "YnabError " invalid
