{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.ChannelsList where

import Control.Applicative
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy.Internal

import Slack.Channel

data ChannelsList = ChannelsList
                  { channelsListOk :: Bool
                  , channelsListChannels :: [Channel]
                  } deriving (Show, Eq)

instance FromJSON ChannelsList where
  parseJSON (Object v) = ChannelsList
    <$> v .: "ok"
    <*> v .: "channels"

instance ToJSON ChannelsList where
  toJSON (ChannelsList ok channels) = object
    [ "ok" .= ok
    , "channels" .= channels
    ]

parseChannelsList :: ByteString -> ChannelsList
parseChannelsList json = channelsList
  where
    maybeChannelsList = decode json :: Maybe ChannelsList
    channelsList = fromMaybe (error "Parse Error") maybeChannelsList

fromChannelName :: ChannelsList -> String -> Maybe Channel
fromChannelName result name
  | length hits > 0 = Just $ hits !! 0
  | otherwise = Nothing
  where 
    hits = filter (\x -> (channelName x) == name) $ channelsListChannels result
