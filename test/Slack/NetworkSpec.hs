{-# LANGUAGE CPP #-}
module Slack.NetworkSpec where

import Slack.Network
import Test.Hspec

spec :: Spec
spec = do

  describe "getJsonFromUsersHistory" $ do
    it "should return json of users list" $ do
      pending

  describe "getJsonFromGroupsHistory" $ do
    it "should return json of groups list" $ do
      pending

  describe "getJsonFromChannelsHistory" $ do
    it "should return json of channels list" $ do
      pending

  describe "getJsonFromGroupsList" $ do
    it "should return json of groups history" $ do
      pending

  describe "dateToEpoch" $ do
    it "should return epoch text from date text" $ do
      dateToEpoch "20150101" `shouldBe` "1420038000.000000000000"
      dateToEpoch "20160101" `shouldBe` "1451574000.000000000000"
      dateToEpoch "20160801" `shouldBe` "1469977200.000000000000"

  describe "zonedToDate" $ do
    it "should return epoch text from date text" $ do
      pending

