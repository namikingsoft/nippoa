{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.Profile
  ( Profile(..)
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

data Profile
  = Profile
  { profileRealName :: String
  , profileImage24 :: String
  , profileBotId :: Maybe String
  } deriving (Show, Eq)

instance FromJSON Profile where
  parseJSON (Object v) = Profile
    <$> v .:  "real_name"
    <*> v .:  "image_24"
    <*> v .:? "bot_id"

instance ToJSON Profile where
  toJSON (Profile name image24 botId) = object
    [ "real_name" .= name
    , "image_24" .= image24
    , "bot_id"   .= botId
    ]
