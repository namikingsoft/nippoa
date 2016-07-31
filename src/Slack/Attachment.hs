{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.Attachment where

import Control.Applicative
import Data.Aeson
import Text.Regex

data Attachment = Attachment
                { attachmentFallback :: String
                , attachmentTitle :: Maybe String
                , attachmentText :: Maybe String
                } deriving (Show, Eq)

instance FromJSON Attachment where
  parseJSON (Object v) = Attachment
    <$> v .: "fallback"
    <*> v .:? "title"
    <*> v .:? "text"

instance ToJSON Attachment where
  toJSON (Attachment fallback title text) = object
    [ "fallback" .= fallback
    , "title" .= title
    , "text" .= text
    ]

attachmentHeadline :: Attachment -> String
attachmentHeadline =
    toMarkdownWithLabel . toMarkdownOnlyLink . attachmentFallback
  where
    toMarkdownWithLabel x = subRegex regexWithLabel x "[\\2](\\1)"
    toMarkdownOnlyLink x = subRegex regexOnlyLink x "[\\1](\\1)"
    regexWithLabel = mkRegex "<([^>\\|]+)\\|([^>]+)>"
    regexOnlyLink = mkRegex "<([^>\\|]+)>"
