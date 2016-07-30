{-# LANGUAGE OverloadedStrings #-}

module Slack.Message where

import Control.Applicative
import Data.Aeson

import Slack.Attachment

data Message = Message
             { messageType :: String
             , messageUser :: Maybe String
             , messageText :: Maybe String
             , messageAttachments :: Maybe [Attachment]
             } deriving (Show, Eq)

instance FromJSON Message where
  parseJSON (Object v) = Message
    <$> v .: "type"
    <*> v .:? "user"
    <*> v .:? "text"
    <*> v .:? "attachments"

instance ToJSON Message where
  toJSON (Message type' user text attachments) = object
    [ "type" .= type'
    , "user" .= user
    , "text" .= text
    , "attachments" .= attachments
    ]
