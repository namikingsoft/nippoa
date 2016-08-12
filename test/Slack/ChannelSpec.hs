{-# LANGUAGE CPP #-}
module Slack.ChannelSpec where

import Slack.Channel
import Test.Hspec

channel0 :: Channel
channel0
  = Channel
  { channelId = "id0"
  , channelName = "general"
  , channelIsGroup = Nothing
  }

channel1 :: Channel
channel1
  = Channel
  { channelId = "id1"
  , channelName = "random"
  , channelIsGroup = Nothing
  }

channel2 :: Channel
channel2
  = Channel
  { channelId = "id2"
  , channelName = "private"
  , channelIsGroup = Just True
  }

spec :: Spec
spec = do

  describe "channelId" $ do
    it "should return initial value" $ do
      channelId channel0 `shouldBe` "id0"
      channelId channel1 `shouldBe` "id1"
      channelId channel2 `shouldBe` "id2"

  describe "channelName" $ do
    it "should return initial value" $ do
      channelName channel0 `shouldBe` "general"
      channelName channel1 `shouldBe` "random"
      channelName channel2 `shouldBe` "private"

  describe "channelIsGroup" $ do
    it "should return initial value" $ do
      channelIsGroup channel0 `shouldBe` Nothing
      channelIsGroup channel1 `shouldBe` Nothing
      channelIsGroup channel2 `shouldBe` Just True
