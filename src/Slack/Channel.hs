{-# LANGUAGE OverloadedStrings #-}

module Slack.Channel
  ( Channel(..),
  ) where

import Control.Applicative
import Data.Aeson

data Channel = Channel
  { id :: String
  , name :: String
  } deriving (Show, Eq)

instance FromJSON Channel where
  parseJSON (Object v) = Channel
    <$> v .: "id"
    <*> v .: "name"

instance ToJSON Channel where
  toJSON (Channel id name) = object
    [ "id"   .= id
    , "name" .= name
    ]
