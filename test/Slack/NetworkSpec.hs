{-# LANGUAGE CPP #-}
module Slack.NetworkSpec where

import Slack.Network
import Test.Hspec

import Data.Time.Format
import Data.Time.Clock

spec :: Spec
spec = do

  describe "getJsonFromGroupsHistory" $ do
    it "should return json of groups history" $ do
      pending

  describe "getJsonFromGroupsList" $ do
    it "should return json of groups history" $ do
      pending

  describe "dateToEpoch" $ do
    it "should return epoch text from date text" $ do
      dateToEpoch "20150101" `shouldBe` "1420038000.000000000000"
      dateToEpoch "20160801" `shouldBe` "1469977200.000000000000"

  describe "utcToEpoch" $ do
    it "should return epoch text from date text" $ do
      let date = "20160101+0900"
          utc = parseTimeOrError True defaultTimeLocale "%Y%m%d%z" date :: UTCTime
      utcToEpoch utc `shouldBe` "1451574000.000000000000"

