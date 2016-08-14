{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.User
  ( User(..)
  ) where

import Data.Aeson
  ( Value(..)
  , FromJSON(..)
  , ToJSON(..)
  , object
  , (.:)
  , (.=)
  )
import Slack.Profile
  ( Profile(..)
  )

data User
  = User
  { userId :: String
  , userName :: String
  , userProfile :: Profile
  } deriving (Show, Eq)

instance FromJSON User where
  parseJSON (Object v) = User
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "profile"

instance ToJSON User where
  toJSON (User id name profile) = object
    [ "id"      .= id
    , "name"    .= name
    , "profile" .= profile
    ]
