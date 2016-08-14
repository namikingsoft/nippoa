{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.Channel
  ( Channel(..)
  ) where

import Data.Aeson
  ( Value(..)
  , FromJSON(..)
  , ToJSON(..)
  , object
  , (.:)
  , (.:?)
  , (.=)
  )

data Channel
  = Channel
  { channelId :: String
  , channelName :: String
  , channelIsGroup :: Maybe Bool
  } deriving (Show, Eq)

instance FromJSON Channel where
  parseJSON (Object v) = Channel
    <$> v .: "id"
    <*> v .: "name"
    <*> v .:? "is_group"

instance ToJSON Channel where
  toJSON (Channel id name isGroup) = object
    [ "id"   .= id
    , "name" .= name
    , "is_group" .= isGroup
    ]
