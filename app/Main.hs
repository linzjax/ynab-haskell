{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Network.HTTP.Simple
import System.Environment (lookupEnv)

import Client (getUser, getBudgets)

main :: IO ()
main = do
  user <- getUser
  print user
  budget <- getBudgets
  print budget
