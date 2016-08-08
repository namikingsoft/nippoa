{-# LANGUAGE CPP #-}
module Slack.Network
  ( getJsonFromGroupsList
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

getJsonFromGroupsList :: String -> IO ByteString
getJsonFromGroupsList token =
    simpleHttp . printf urlGroupsList $ token
  where
    urlGroupsList = "https://slack.com/api/groups.list?token=%s"

getJsonFromGroupsHistory :: String -> String -> String -> Bool -> IO ByteString
getJsonFromGroupsHistory token channel date isToday =
    simpleHttp $ printf urlGroupsHistory token channel oldest latest
  where
    urlGroupsHistory =
      "https://slack.com/api/groups.history?" ++
      "token=%s&channel=%s&count=1000&oldest=%s&latest=%s"
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
