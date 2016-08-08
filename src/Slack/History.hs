{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.History
  ( History(..)
  , parseHistory
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
import Slack.Message
  ( Message(..)
  )

data History
  = History
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
