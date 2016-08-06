{-# LANGUAGE CPP #-}
module Nippoa.Value.TimeStampSpec where

import Nippoa.Value.TimeStamp
import Test.Hspec

import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Clock

zonedtime0 :: ZonedTime
zonedtime0 = utcToZonedTime jst . utcFromTs $ "2016-07-29 14:01:18"
  where
    utcFromTs x = parseTimeOrError True defaultTimeLocale "%F %T" x :: UTCTime
    jst = hoursToTimeZone 9

timestamp0 :: TimeStamp
timestamp0 = TimeStamp zonedtime0

spec :: Spec
spec = do

  describe "timeStampZonedTime" $ do
    it "should return initial value" $ do
      show (timeStampZonedTime timestamp0) `shouldBe` show zonedtime0

  describe "timeStampFromTs" $ do
    it "should return new instance from timestamp of slack" $ do
      let format = formatTime defaultTimeLocale "%F %T"
          zonedtime = timeStampZonedTime . timeStampFromTs $ "1469768478.000747"
      format zonedtime `shouldBe` "2016-07-29 14:01:18"
