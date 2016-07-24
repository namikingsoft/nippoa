{-# LANGUAGE OverloadedStrings #-}

module Slack.ChannelList where

import Control.Applicative
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy.Internal

import Slack.Channel

data ChannelList = ChannelList
                 { channelListOk :: Bool
                 , channelListChannels :: [Channel]
                 } deriving (Show, Eq)

instance FromJSON ChannelList where
  parseJSON (Object v) = ChannelList
    <$> v .: "ok"
    <*> v .: "channels"

instance ToJSON ChannelList where
  toJSON (ChannelList ok channels) = object
    [ "ok" .= ok
    , "channels" .= channels
    ]

parse :: ByteString -> ChannelList
parse json = channelList
  where
    maybeChannelList = decode json :: Maybe ChannelList
    channelList = fromMaybe (error "Parse Error") maybeChannelList

fromName :: ChannelList -> String -> Maybe Channel
fromName result name
  | length hits > 0 = Just $ hits !! 0
  | otherwise = Nothing
  where 
    hits = filter (\x -> (channelName x) == name) $ channelListChannels result
