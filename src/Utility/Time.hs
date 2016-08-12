{-# LANGUAGE CPP #-}
module Utility.Time
  ( zonedToDate
  , dateToUtc
  , utcToEpoch
  , diffSec
  , jst
  ) where

import Data.Time.LocalTime
  ( ZonedTime
  , TimeZone
  , hoursToTimeZone
  )
import Data.Time.Format
  ( formatTime
  , parseTimeOrError
  , defaultTimeLocale
  )
import Data.Time.Clock
  ( UTCTime
  , NominalDiffTime
  )

zonedToDate :: ZonedTime -> String
zonedToDate = formatTime defaultTimeLocale "%Y%m%d"

dateToUtc :: String -> UTCTime
dateToUtc x =
  parseTimeOrError True defaultTimeLocale "%Y%m%d%z" (x++"+0900") :: UTCTime

utcToEpoch :: UTCTime -> String
utcToEpoch = formatTime defaultTimeLocale "%s.%q"

diffSec :: NominalDiffTime -> NominalDiffTime
diffSec x = x

jst :: TimeZone
jst = hoursToTimeZone 9
