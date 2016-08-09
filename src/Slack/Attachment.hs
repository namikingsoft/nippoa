{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.Attachment
  ( Attachment(..)
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
  , (.:)
  , (.:?)
  , (.=)
  )

data Attachment
  = Attachment
  { attachmentFallback :: String
  , attachmentTitle :: Maybe String
  , attachmentTitleLink :: Maybe String
  , attachmentText :: Maybe String
  , attachmentPreText :: Maybe String
  } deriving (Show, Eq)

instance FromJSON Attachment where
  parseJSON (Object v) = Attachment
    <$> v .: "fallback"
    <*> v .:? "title"
    <*> v .:? "title_link"
    <*> v .:? "text"
    <*> v .:? "pretext"

instance ToJSON Attachment where
  toJSON (Attachment fallback title titleLink text pretext) = object
    [ "fallback" .= fallback
    , "title" .= title
    , "title_link" .= titleLink
    , "text" .= text
    , "pretext" .= text
    ]
