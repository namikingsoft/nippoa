{-# LANGUAGE DeriveGeneric #-}

module Slack.ChannelList where

import GHC.Generics
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy.Internal

import Slack.Channel

data ChannelList = ChannelList
  { ok :: Bool
  , channels :: [Channel]
  } deriving (Show, Eq, Generic)

instance FromJSON ChannelList
instance ToJSON ChannelList

parse :: ByteString -> ChannelList
parse json = channelList
  where
    maybeChannelList = decode json :: Maybe ChannelList
    channelList = fromMaybe (error "Parse Error") maybeChannelList

fromName :: ChannelList -> String -> Maybe Channel
fromName result str
  | length hits > 0 = Just $ hits !! 0
  | otherwise = Nothing
  where 
    hits = filter (\x -> (name x) == str) $ channels result
