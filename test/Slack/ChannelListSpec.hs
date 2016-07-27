module Slack.ChannelListSpec where

import Slack.ChannelList
import Slack.Channel
import Test.Hspec

import Prelude hiding (id)
import Control.Exception (evaluate)
import Data.ByteString.Lazy.Char8 (pack)
import Slack.ChannelSpec

channelList :: ChannelList
channelList = ChannelList
            { channelListOk = True
            , channelListChannels = [channel0, channel1]
            }

spec :: Spec
spec = do

  describe "channelListOk" $ do
    it "should return initial value" $ do
      channelListOk channelList `shouldBe` True

  describe "channelListChannels" $ do
    it "should return initial value" $ do
      let list = channelListChannels channelList
      length list `shouldBe` 2
      list !! 0 `shouldBe` channel0
      list !! 1 `shouldBe` channel1

  describe "parseChannelList" $ do
    it "should return parsed from json" $ do
      let json = "{\"ok\": true, \"channels\": [{\"id\": \"id0\", \"name\": \"general\"}, {\"id\": \"id1\", \"name\": \"random\"}]}"
      parseChannelList (pack json) `shouldBe` channelList
    it "should call error when parse error" $ do
      let json = "{\"okk\": true}"
      evaluate (parseChannelList $ pack json) `shouldThrow` anyErrorCall

  describe "fromChannelName" $ do
    it "should return maybe channel" $ do
      fromChannelName channelList "general" `shouldBe` Just channel0
      fromChannelName channelList "random"  `shouldBe` Just channel1
      fromChannelName channelList "nothing" `shouldBe` Nothing
