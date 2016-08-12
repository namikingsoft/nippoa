{-# LANGUAGE CPP #-}
module Utility.TimeSpec where

import Utility.Time
import Test.Hspec

import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Clock

utctime0 :: UTCTime
utctime0 =
    parseTimeOrError True defaultTimeLocale "%Y%m%d%z" date :: UTCTime
  where
    date = "20150101+0900"

zonedtime0 :: ZonedTime
zonedtime0 = utcToZonedTime jst utctime0

spec :: Spec
spec = do

  describe "zonedToDate" $ do
    it "should return epoch text from date text" $ do
      zonedToDate zonedtime0 `shouldBe` "20150101"

  describe "dateToUtc" $ do
    it "should return epoch text from date text" $ do
      dateToUtc "20150101" `shouldBe` utctime0

  describe "utcToEpoch" $ do
    it "should return epoch text from date text" $ do
      (utcToEpoch . dateToUtc) "20150101" `shouldBe` "1420038000.000000000000"
      (utcToEpoch . dateToUtc) "20160101" `shouldBe` "1451574000.000000000000"
      (utcToEpoch . dateToUtc) "20160801" `shouldBe` "1469977200.000000000000"

  describe "diffSec" $ do
    it "should return seconds of nominal diff time" $ do
      diffSec 86400 `shouldBe` 86400

  describe "jst" $ do
    it "should return timezone of jst" $ do
      jst `shouldBe` hoursToTimeZone 9

