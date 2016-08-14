{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.Message
  ( Message(..)
  , messageDateTime
  , messageTemplate
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
import Data.Maybe
  ( fromMaybe
  )
import Data.Time.LocalTime
  ( ZonedTime
  , hoursToTimeZone
  , utcToZonedTime
  )
import Data.Time.Format
  ( formatTime
  , parseTimeOrError
  , defaultTimeLocale
  )
import Data.Time.Clock
  ( UTCTime
  )
import Text.Regex
  ( mkRegex
  , subRegex
  , splitRegex
  )
import Codec.Binary.UTF8.String
  ( encodeString
  , decodeString
  )
import Slack.Attachment
  ( Attachment(..)
  )
import Utility.Time
  ( jst
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

messageDateTime :: Message -> ZonedTime
messageDateTime = zonedTime . utcTime . messageTs
  where
    zonedTime x = utcToZonedTime jst x
    utcTime x = parseTimeOrError True defaultTimeLocale "%s%Q" x :: UTCTime

messageTemplate :: Message -> String
messageTemplate x =
    time x ++ text x ++ "\n" ++ attachments x
  where
    formatDate = formatTime defaultTimeLocale "[%F %T] "
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
      "> " ++ (toMarkdown . attachmentFallback) x ++ "\n" ++
      "\n" ++ (pre . toMarkdown . justAttachmentText) x ++ "\n"
    pre :: String -> String
    pre x
      | x == "" = x
      | otherwise = concat . map addSite . splitLine $ x
      where
        splitLine = splitRegex (mkRegex "\n")
        addSite x = "> " ++ x ++ "\n" :: String

toMarkdown :: String -> String
toMarkdown =
    decodeString . toImageIfExists . toMarkdownOnlyLink . toMarkdownWithLabel . encodeString
  where
    toMarkdownWithLabel x = subRegex regexWithLabel x "[\\2](\\1)"
    toMarkdownOnlyLink x = subRegex regexOnlyLink x "[\\1](\\1)"
    toImageIfExists x = subRegex regexWithImage x "!\\1"
    regexWithLabel = mkRegex "<([^<>\\|]+)\\|([^<>]+)>"
    regexOnlyLink = mkRegex "<([^<>\\|]+)>"
    regexWithImage = mkRegex "(\\[[^\\]*\\]\\([^\\)]+\\.(gif|png|jpe?g)\\))"
