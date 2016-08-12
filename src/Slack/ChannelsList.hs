{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.ChannelsList
  ( ChannelsList(..)
  , parseChannelsList
  , channelByChannelsName
  ) where

import Control.Applicative
  ( (<$>)
  , (<*>)
  )
import Data.Aeson
  ( Value(..)
  , FromJSON(..)
  , ToJSON(..)
  , object
  , decode
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.ByteString.Lazy.Internal
  ( ByteString
  )
import Slack.Channel
  ( Channel(..)
  )

data ChannelsList
  = ChannelsList
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

channelByChannelsName :: ChannelsList -> String -> Maybe Channel
channelByChannelsName this name
  | length hits > 0 = Just $ hits !! 0
  | otherwise = Nothing
  where 
    hits = filter (\x -> name == channelName x) $ channelsListChannels this
