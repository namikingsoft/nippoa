{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.Message where

import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Clock
import Data.Aeson
import Text.Regex

import Slack.Attachment

data Message = Message
             { messageTs :: String
             , messageType :: String
             , messageUser :: Maybe String
             , messageText :: Maybe String
             , messageAttachments :: Maybe [Attachment]
             } deriving (Show, Eq)

instance FromJSON Message where
  parseJSON (Object v) = Message
    <$> v .: "ts"
    <*> v .: "type"
    <*> v .:? "user"
    <*> v .:? "text"
    <*> v .:? "attachments"

instance ToJSON Message where
  toJSON (Message ts type' user text attachments) = object
    [ "ts" .= ts
    , "type" .= type'
    , "user" .= user
    , "text" .= text
    , "attachments" .= attachments
    ]

messageDateTime :: Message -> ZonedTime
messageDateTime = zonedTime . utcTime . messageTs
  where
    zonedTime x = utcToZonedTime jst x
    utcTime x = readTime defaultTimeLocale "%s%Q" x :: UTCTime
    jst = hoursToTimeZone 9

toMarkdown :: String -> String
toMarkdown=
    toMarkdownOnlyLink . toMarkdownWithLabel
  where
    toMarkdownWithLabel x = subRegex regexWithLabel x "[\\2](\\1)"
    toMarkdownOnlyLink x = subRegex regexOnlyLink x "[\\1](\\1)"
    regexWithLabel = mkRegex "<([^<>\\|]+)\\|([^<>]+)>"
    regexOnlyLink = mkRegex "<([^<>\\|]+)>"
