{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.Message
  ( Message(..)
  , toMarkdown
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
import Slack.Attachment
  ( Attachment(..)
  )
import Utility.Regex
  ( replace
  )

data Message
  = Message
  { messageTs :: String
  , messageType :: String
  , messageUser :: Maybe String
  , messageText :: Maybe String
  , messageBotId :: Maybe String
  , messageAttachments :: Maybe [Attachment]
  } deriving (Show, Eq)

instance FromJSON Message where
  parseJSON (Object v) = Message
    <$> v .: "ts"
    <*> v .: "type"
    <*> v .:? "user"
    <*> v .:? "text"
    <*> v .:? "bot_id"
    <*> v .:? "attachments"

instance ToJSON Message where
  toJSON (Message ts type' user text botId attachments) = object
    [ "ts" .= ts
    , "type" .= type'
    , "user" .= user
    , "text" .= text
    , "bot_id" .= botId
    , "attachments" .= attachments
    ]

toMarkdown :: String -> String
toMarkdown =
    toImageIfExists . toMarkdownOnlyLink . toMarkdownWithLabel
  where
    toMarkdownWithLabel = replaceWithLabel "[\\2](\\1)"
    toMarkdownOnlyLink = replaceOnlyLink "[\\1](\\1)"
    toImageIfExists = replaceWithImage "!\\1"
    replaceWithLabel = replace "<([^<>\\|]+)\\|([^<>]+)>"
    replaceOnlyLink = replace "<([^<>\\|]+)>"
    replaceWithImage = replace "(\\[[^\\]*\\]\\([^\\)]+\\.(gif|png|jpe?g)\\))"
