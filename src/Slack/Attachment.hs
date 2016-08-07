{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.Attachment where

import Control.Applicative
import Data.Aeson

data Attachment = Attachment
                { attachmentFallback :: String
                , attachmentTitle :: Maybe String
                , attachmentTitleLink :: Maybe String
                , attachmentText :: Maybe String
                } deriving (Show, Eq)

instance FromJSON Attachment where
  parseJSON (Object v) = Attachment
    <$> v .: "fallback"
    <*> v .:? "title"
    <*> v .:? "title_link"
    <*> v .:? "text"

instance ToJSON Attachment where
  toJSON (Attachment fallback title titleLink text) = object
    [ "fallback" .= fallback
    , "title" .= title
    , "title_link" .= titleLink
    , "text" .= text
    ]
