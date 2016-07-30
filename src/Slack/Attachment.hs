{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.Attachment where

import Control.Applicative
import Data.Aeson

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
