{-# LANGUAGE OverloadedStrings #-}
module Models.Category
  ( Category(..)
  , CategoryGroup(..)
  , CategoriesResponse(..)
  -- , AccountsSummaryResponse(..)
  -- , AccountDetailResponse(..)
  ) where

import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)


data CategoriesResponse = CategoriesResponse [CategoryGroup] deriving Show

instance FromJSON CategoriesResponse where
  parseJSON (Object o) = do
    respObj <- o .: "data"
    cgObj <- respObj .: "category_groups"
    return $ CategoriesResponse cgObj
  parseJSON invalid = typeMismatch "CategoriesResponse" invalid

data CategoryGroup = CategoryGroup
  { cgId      :: !Text
  , cgName    :: !Text
  , cgHidden  :: !Bool
  , cgDeleted :: !Bool
  } deriving (Show)

instance FromJSON CategoryGroup where
  parseJSON (Object o) = CategoryGroup <$>
      o .: "id" <*>
      o .: "name" <*>
      o .: "hidden" <*>
      o .: "deleted"
  parseJSON invalid = typeMismatch "CategoryGroup" invalid

data Category = Category
  { categoryId                      :: !Text
  , categoryCGId                    :: !Text -- | TODO: This should be a reference to a CategoryGroup
  , categoryName                    :: !Text
  , categoryHidden                  :: !Bool
  , categoryOriginalCGId            :: !(Maybe Text) -- | TODO: Wait.. what?
  , categoryNote                    :: !(Maybe Text)
  , categoryBudgeted                :: !Int
  , categoryActivity                :: !Int
  , categoryBalance                 :: !Int
  , categoryGoalType                :: !(Maybe Text) -- | TODO: What is this?
  , categoryGoalCreationMonth       :: !(Maybe Text)
  , categoryGoalTarget              :: !(Maybe Int)
  , categoryGoalTargetMonth         :: !(Maybe Text)
  , categoryGoalPercentageCompleted :: !(Maybe Int)
  , categoryDeleted                 :: !Bool
  } deriving (Show)

instance FromJSON Category where
  parseJSON (Object o) = Category <$>
    o .: "id" <*>
    o .: "category_group_id" <*>
    o .: "name" <*>
    o .: "hidden" <*>
    o .:? "original_category_group_id" <*>
    o .:? "note" <*>
    o .: "budgeted" <*>
    o .: "activity" <*>
    o .: "balance" <*>
    o .:? "goal_type" <*>
    o .:? "goal_creation_month" <*>
    o .:? "goal_target" <*>
    o .:? "goal_target_month" <*>
    o .:? "goal_percentage_complete" <*>
    o .: "deleted"
  parseJSON invalid = typeMismatch "Category" invalid
