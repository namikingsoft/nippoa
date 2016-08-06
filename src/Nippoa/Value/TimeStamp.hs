{-# LANGUAGE CPP #-}
module Nippoa.Value.TimeStamp
  ( TimeStamp (..)
  , timeStampFromTs
  ) where

import Data.Time.LocalTime
  ( ZonedTime
  , utcToZonedTime
  , hoursToTimeZone
  )
import Data.Time.Format
  ( parseTimeOrError
  , defaultTimeLocale
  )
import Data.Time.Clock
  ( UTCTime
  )

data TimeStamp = TimeStamp
               { timeStampZonedTime :: ZonedTime
               } deriving (Show)

timeStampFromTs :: String -> TimeStamp
timeStampFromTs = TimeStamp . utcToZonedTime jst . utcFromTs
  where
    utcFromTs x = parseTimeOrError True defaultTimeLocale "%s%Q" x :: UTCTime
    jst = hoursToTimeZone 9
