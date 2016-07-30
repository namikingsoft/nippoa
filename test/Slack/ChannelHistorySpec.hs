{-# LANGUAGE CPP #-}
module Slack.ChannelHistorySpec where

import Slack.ChannelHistory
import Slack.Message
import Test.Hspec

import System.FilePath.Posix
import Control.Exception (evaluate)
import Data.ByteString.Lazy.Char8 (pack)
import Slack.MessageSpec

channelHistory :: ChannelHistory
channelHistory = ChannelHistory
               { channelHistoryOk = True
               , channelHistoryMessages = [message0, message1]
               }

spec :: Spec
spec = do

  describe "channelHistoryOk" $ do
    it "should return initial value" $ do
      channelHistoryOk channelHistory `shouldBe` True

  describe "channelHistoryMessages" $ do
    it "should return initial value" $ do
      let messages = channelHistoryMessages channelHistory
      length messages `shouldBe` 2
      messages !! 0 `shouldBe` message0
      messages !! 1 `shouldBe` message1

  describe "parseChannelHistory" $ do
    it "should return parsed from json" $ do
      let jsonPath = joinPath [takeDirectory __FILE__,"mock","ChannelHistory.json"]
      json <- readFile jsonPath
      parseChannelHistory (pack json) `shouldBe` channelHistory
    it "should call error when parse error" $ do
      let json = pack "{\"okk\": true}"
      evaluate (parseChannelHistory json) `shouldThrow` anyErrorCall
