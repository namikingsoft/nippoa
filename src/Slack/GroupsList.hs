{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.GroupsList
  ( GroupsList(..)
  , parseGroupsList
  , channelByGroupsName
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
  , decode
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.ByteString.Lazy.Internal
  ( ByteString
  )
import Slack.Channel
  ( Channel(..)
  )

data GroupsList
  = GroupsList
  { groupsListOk :: Bool
  , groupsListGroups :: [Channel]
  } deriving (Show, Eq)

instance FromJSON GroupsList where
  parseJSON (Object v) = GroupsList
    <$> v .: "ok"
    <*> v .: "groups"

instance ToJSON GroupsList where
  toJSON (GroupsList ok groups) = object
    [ "ok" .= ok
    , "groups" .= groups
    ]

parseGroupsList :: ByteString -> GroupsList
parseGroupsList json = groupsList
  where
    maybeGroupsList = decode json :: Maybe GroupsList
    groupsList = fromMaybe (error "Parse Error") maybeGroupsList

channelByGroupsName :: GroupsList -> String -> Maybe Channel
channelByGroupsName this name
  | length hits > 0 = Just $ hits !! 0
  | otherwise = Nothing
  where 
    hits = filter (\x -> name == channelName x) $ groupsListGroups this
