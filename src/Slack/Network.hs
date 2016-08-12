{-# LANGUAGE CPP #-}
module Slack.Network
  ( getJsonFromUsersList
  , getJsonFromGroupsList
  , getJsonFromChannelsList
  , getJsonFromGroupsHistory
  , dateToEpoch
  , zonedToDate
  ) where

import Text.Printf
  ( printf
  )
import Data.ByteString.Lazy.Internal
  ( ByteString
  )
import Network.HTTP.Conduit
  ( simpleHttp
  )
import Data.Time.LocalTime
  ( ZonedTime
  )
import Data.Time.Format
  ( formatTime
  , parseTimeOrError
  , defaultTimeLocale
  )
import Data.Time.Clock
  ( UTCTime
  )
import Slack.Channel
  ( Channel(..)
  )

getJsonFromUsersList :: String -> IO ByteString
getJsonFromUsersList = simpleHttp . printf url
  where
    url = "https://slack.com/api/users.list?token=%s"

getJsonFromGroupsList :: String -> IO ByteString
getJsonFromGroupsList = simpleHttp . printf url
  where
    url = "https://slack.com/api/groups.list?token=%s"

getJsonFromChannelsList :: String -> IO ByteString
getJsonFromChannelsList = simpleHttp . printf url
  where
    url = "https://slack.com/api/channels.list?token=%s"

getJsonFromGroupsHistory :: String -> Channel -> String -> Bool -> IO ByteString
getJsonFromGroupsHistory token channel date isToday =
  case channelIsGroup channel of
    Just isGroup | isGroup ->
      simpleHttp $ printf urlGroups token (channelId channel) oldest latest
    otherwise ->
      simpleHttp $ printf urlChannels token (channelId channel) oldest latest
  where
    urlGroups =
      "https://slack.com/api/groups.history" ++ params
    urlChannels =
      "https://slack.com/api/channels.history" ++ params
    params =
      "?token=%s&channel=%s&count=1000&oldest=%s&latest=%s"
    oldest = dateToEpoch date
    latest
      | isToday = ""
      | otherwise = dateToEpoch $ show $ (read date :: Int) + 1

dateToEpoch :: String -> String
dateToEpoch =
    utcToEpoch . dateToUTCTime
  where
    dateToUTCTime x =
      parseTimeOrError True defaultTimeLocale "%Y%m%d%z" (x++"+0900") :: UTCTime
    utcToEpoch = formatTime defaultTimeLocale "%s.%q"

zonedToDate :: ZonedTime -> String
zonedToDate = formatTime defaultTimeLocale "%Y%m%d"
