{-# LANGUAGE OverloadedStrings #-}

module Slack.Message where

import Control.Applicative
import Data.Aeson

data Message = Message
             { messageType :: String
             , messageUser :: Maybe String
             , messageText :: Maybe String
             } deriving (Show, Eq)

instance FromJSON Message where
  parseJSON (Object v) = Message
    <$> v .: "type"
    <*> v .:? "user"
    <*> v .:? "text"

instance ToJSON Message where
  toJSON (Message type' user text) = object
    [ "type" .= type'
    , "user" .= user
    , "text" .= text
    ]
