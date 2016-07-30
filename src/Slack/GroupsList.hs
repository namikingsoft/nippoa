{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.GroupsList where

import Control.Applicative
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy.Internal

import Slack.Channel

data GroupsList = GroupsList
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

fromGroupsName :: GroupsList -> String -> Maybe Channel
fromGroupsName result name
  | length hits > 0 = Just $ hits !! 0
  | otherwise = Nothing
  where 
    hits = filter (\x -> (channelName x) == name) $ groupsListGroups result
