{-# LANGUAGE CPP #-}
module Slack.ChannelsListSpec where

import Slack.ChannelsList
import Test.Hspec

import System.FilePath.Posix
import Control.Exception (evaluate)
import Data.ByteString.Lazy.Char8 (pack)
import Slack.ChannelSpec

channelsList0 :: ChannelsList
channelsList0
  = ChannelsList
  { channelsListOk = True
  , channelsListChannels = [channel0, channel1]
  }

spec :: Spec
spec = do

  describe "channelsListOk" $ do
    it "should return initial value" $ do
      channelsListOk channelsList0 `shouldBe` True

  describe "channelsListChannels" $ do
    it "should return initial value" $ do
      let list = channelsListChannels channelsList0
      length list `shouldBe` 2
      list !! 0 `shouldBe` channel0
      list !! 1 `shouldBe` channel1

  describe "parseChannelsList" $ do
    it "should return parsed from json" $ do
      let jsonPath = joinPath [takeDirectory __FILE__,"mock","ChannelsList.json"]
      json <- readFile jsonPath
      parseChannelsList (pack json) `shouldBe` channelsList0
    it "should call error when parse error" $ do
      let json = "{\"okk\": true}"
      evaluate (parseChannelsList $ pack json) `shouldThrow` anyErrorCall

  describe "channelByChannelsName" $ do
    it "should return maybe channel by name" $ do
      channelByChannelsName channelsList0 "general" `shouldBe` Just channel0
      channelByChannelsName channelsList0 "random"  `shouldBe` Just channel1
      channelByChannelsName channelsList0 "nothing" `shouldBe` Nothing
