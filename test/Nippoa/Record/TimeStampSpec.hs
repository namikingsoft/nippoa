{-# LANGUAGE CPP #-}
module Nippoa.Record.TimeStampSpec where

import Nippoa.Record.TimeStamp
import Test.Hspec

import Data.Time.Format
import Data.Time.Clock

utctime0 :: UTCTime
utctime0 = utcFromTs "2016-07-29 05:01:18"
  where
    utcFromTs x = parseTimeOrError True defaultTimeLocale "%F %T" x :: UTCTime

timestamp0 :: TimeStamp
timestamp0 = TimeStamp utctime0

spec :: Spec
spec = do

  describe "timeStampUTCTime" $ do
    it "should return initial value" $ do
      timeStampUTCTime timestamp0 `shouldBe` utctime0

  describe "timeStampFromTs" $ do
    it "should return new instance from timestamp of slack" $ do
      let format = formatTime defaultTimeLocale "%F %T"
          utctime = timeStampUTCTime . timeStampFromTs $ "1469768478.000747"
      format utctime `shouldBe` "2016-07-29 05:01:18"

  describe "timeStampFromTs" $ do
    it "should return timestamp text of jst" $ do
      timeStampToText timestamp0 `shouldBe` "2016-07-29 14:01:18"
