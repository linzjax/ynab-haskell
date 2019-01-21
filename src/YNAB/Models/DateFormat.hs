{-# LANGUAGE OverloadedStrings #-}
module YNAB.Models.DateFormat ( DateFormat(..)) where

--
import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

-- | Object for storing the date format.
-- | TODO: actually use this to format dates correctly
-- | parseTimeM True defaultTimeLocale "%-m/%-d/%Y" "4/30/2018" :: Maybe Day
newtype DateFormat = DateFormat { dfFormat :: Text } deriving (Show)

instance FromJSON DateFormat where
  parseJSON (Object o) = DateFormat <$> o .: "format"
  parseJSON invalid = typeMismatch "DateFormat" invalid
