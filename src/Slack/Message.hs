{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.Message where

import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Clock
import Data.Aeson
import Data.Maybe
import Text.Regex
import Codec.Binary.UTF8.String (encodeString, decodeString)

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
    utcTime x = parseTimeOrError True defaultTimeLocale "%s%Q" x :: UTCTime
    jst = hoursToTimeZone 9

toMarkdown :: String -> String
toMarkdown =
    decodeString . toMarkdownOnlyLink . toMarkdownWithLabel . encodeString
  where
    toMarkdownWithLabel x = subRegex regexWithLabel x "[\\2](\\1)"
    toMarkdownOnlyLink x = subRegex regexOnlyLink x "[\\1](\\1)"
    regexWithLabel = mkRegex "<([^<>\\|]+)\\|([^<>]+)>"
    regexOnlyLink = mkRegex "<([^<>\\|]+)>"

messageTemplate :: Message -> String
messageTemplate x =
    time x ++ "  " ++ text x ++ "\n" ++ attachments x
  where
    formatDate = formatTime defaultTimeLocale "%F %T"
    time = formatDate . messageDateTime
    text = toMarkdown . fromMaybe "" . messageText
    justAttachmentText x
      | normal x == attachmentFallback x = ""
      | otherwise = normal x
      where
        normal = fromMaybe "" . attachmentText
    justAttachments = fromMaybe [] . messageAttachments
    attachments = concat . map attachment . justAttachments
    attachment x =
      "> " ++ (toMarkdown . attachmentFallback) x ++
      "\n" ++ (pre . toMarkdown . justAttachmentText) x ++ "\n"
    pre :: String -> String
    pre x
      | x == "" = x
      | otherwise = "```\n" ++ x ++ "\n```"
