{-# LANGUAGE CPP #-}
module Slack.ChannelSpec where

import Slack.Channel
import Test.Hspec

channel0 :: Channel
channel0
  = Channel
  { channelId = "id0"
  , channelName = "general"
  }

channel1 :: Channel
channel1
  = Channel
  { channelId = "id1"
  , channelName = "random"
  }

spec :: Spec
spec = do

  describe "channelId" $ do
    it "should return initial value" $ do
      channelId channel0 `shouldBe` "id0"
      channelId channel1 `shouldBe` "id1"

  describe "name" $ do
    it "should return initial value" $ do
      channelName channel0 `shouldBe` "general"
      channelName channel1 `shouldBe` "random"
