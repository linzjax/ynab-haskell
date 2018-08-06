{-# LANGUAGE OverloadedStrings #-}

module Client (getUser, getBudgets) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, FromJSON(..), Object(..), Value(Object), decode, withObject, (.:))
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe (fromMaybe)
-- import qualified Data.HashMap as M
import Data.HashMap.Strict (toList)
import Data.Text (Text)
-- import qualified Data.Yaml as Yaml
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import GHC.Generics (Generic)
import Network.HTTP.Simple (setRequestHeader, httpLBS, getResponseStatusCode, getResponseHeader, getResponseBody)
import System.Environment (lookupEnv)

data ApiResponse = ApiResponse {
  res :: Value
} deriving (Eq, Show, Generic)

instance FromJSON ApiResponse where
  parseJSON = withObject "ApiResponse" $ \v ->
    ApiResponse <$> v .: "data"

data ApiData = User { userId :: String }
            | Budget { budgetId :: String
                     , budgetName :: String
                     }

            deriving (Eq, Show, Generic)

-- data User = User {
--   userId :: String
-- }

instance FromJSON ApiData where
  parseJSON = withObject "ApiData" $ \v -> do
    case (toList v) of
      [("user", Object v')] -> User <$> v' .: "id"
      [("budgets", Object v')] -> Budget <$> v' .: "id"
                                         <*> v' .: "name"
      _                     -> fail "Rule: unexpect format"

-- getEndpoint :: Request -> Response
getEndpoint requestUrl = do
  apiKey <- S8.pack . fromMaybe "" <$> lookupEnv "API_TOKEN"
  let request = setRequestHeader "Authorization" ["Bearer " <> apiKey]
                $ requestUrl
  response <- httpLBS request
  print $ getResponseStatusCode response
  return response


getUser :: IO (Maybe Value)
getUser = do
  response <- getEndpoint "GET https://api.youneedabudget.com/v1/user"
  case (getResponseStatusCode response) of
    200 -> do
      print $ getResponseHeader "Content-Type" response
      let response' = (decode $ (getResponseBody response) :: Maybe ApiResponse)
      case response' of
        Just response' ->
          return $ Just (res response')
        Nothing -> return Nothing
    _ -> do
      print "Error"
      return Nothing


getBudgets :: IO (Maybe Value)
getBudgets = do
  response <- getEndpoint "GET https://api.youneedabudget.com/v1/budgets"
  case (getResponseStatusCode response) of
    200 -> do
      liftIO $ print $ getResponseHeader "Content-Type" response
      let response' = (decode $ (getResponseBody response) :: Maybe ApiResponse)
      case response' of
        Just response' ->
          return $ Just (res response')
        Nothing -> return Nothing
    _ -> do
      print "Error"
      return Nothing
