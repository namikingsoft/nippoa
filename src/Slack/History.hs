{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.History where

import Control.Applicative
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy.Internal

import Slack.Message

data History = History
             { historyOk :: Bool
             , historyMessages :: [Message]
             } deriving (Show, Eq)

instance FromJSON History where
  parseJSON (Object v) = History
    <$> v .: "ok"
    <*> v .: "messages"

instance ToJSON History where
  toJSON (History ok messages) = object
    [ "ok" .= ok
    , "messages" .= messages
    ]

parseHistory :: ByteString -> History
parseHistory json = channelHistory
  where
    maybeHistory = decode json :: Maybe History
    channelHistory = fromMaybe (error "Parse Error") maybeHistory
