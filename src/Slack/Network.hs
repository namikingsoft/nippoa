{-# LANGUAGE CPP #-}
module Slack.Network where

import Text.Printf
import Data.ByteString.Lazy.Internal
import Network.HTTP.Conduit

getJsonFromGroupsList :: String -> IO ByteString
getJsonFromGroupsList token =
    simpleHttp . printf urlGroupsList $ token
  where
    urlGroupsList = "https://slack.com/api/groups.list?token=%s"

getJsonFromGroupsHistory :: String -> String -> IO ByteString
getJsonFromGroupsHistory token channel =
    simpleHttp $ printf urlGroupsHistory token channel
  where
    urlGroupsHistory = "https://slack.com/api/groups.history?token=%s&channel=%s"
