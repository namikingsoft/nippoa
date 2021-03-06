{-# LANGUAGE CPP #-}
module Nippoa.Record.TimeStamp
  ( TimeStamp (..)
  , timeStampFromTs
  , timeStampToText
  ) where

import Data.Time.LocalTime
  ( ZonedTime
  , utcToZonedTime
  , hoursToTimeZone
  )
import Data.Time.Format
  ( formatTime
  , parseTimeOrError
  , defaultTimeLocale
  )
import Data.Time.Clock
  ( UTCTime
  )
import Utility.Time
  ( jst
  )

newtype TimeStamp
  = TimeStamp
  { timeStampUTCTime :: UTCTime
  } deriving (Show, Eq)

timeStampFromTs :: String -> TimeStamp
timeStampFromTs = TimeStamp . utcFromTs
  where
    utcFromTs x = parseTimeOrError True defaultTimeLocale "%s%Q" x :: UTCTime

timeStampToText :: TimeStamp -> String
timeStampToText = format . utcToZonedTime jst . timeStampUTCTime
  where
    format = formatTime defaultTimeLocale "%F %T"
