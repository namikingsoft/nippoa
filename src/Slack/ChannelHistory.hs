{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.ChannelHistory where

import Control.Applicative
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy.Internal

import Slack.Message

data ChannelHistory = ChannelHistory
                    { channelHistoryOk :: Bool
                    , channelHistoryMessages :: [Message]
                    } deriving (Show, Eq)

instance FromJSON ChannelHistory where
  parseJSON (Object v) = ChannelHistory
    <$> v .: "ok"
    <*> v .: "messages"

instance ToJSON ChannelHistory where
  toJSON (ChannelHistory ok messages) = object
    [ "ok" .= ok
    , "messages" .= messages
    ]

parseChannelHistory :: ByteString -> ChannelHistory
parseChannelHistory json = channelHistory
  where
    maybeChannelHistory = decode json :: Maybe ChannelHistory
    channelHistory = fromMaybe (error "Parse Error") maybeChannelHistory
