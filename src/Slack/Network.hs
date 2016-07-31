{-# LANGUAGE CPP #-}
module Slack.Network where

import Text.Printf
import Data.ByteString.Lazy.Internal
import Network.HTTP.Conduit
import Data.Time.Format
import Data.Time.Clock

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

dateToEpoch :: String -> String
dateToEpoch =
    utcToEpoch . dateToUTCTime
  where
    dateToUTCTime x =
      parseTimeOrError True defaultTimeLocale "%Y%m%d%z" (x++"+0900") :: UTCTime

utcToEpoch :: UTCTime -> String
utcToEpoch = formatTime defaultTimeLocale "%s.%q"
