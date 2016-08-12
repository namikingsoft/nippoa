{-# LANGUAGE CPP #-}
module Slack.HistorySpec where

import Slack.History
import Test.Hspec

import System.FilePath.Posix
import Control.Exception (evaluate)
import Data.ByteString.Lazy.Char8 (pack)
import Slack.MessageSpec

history :: History
history
  = History
  { historyOk = True
  , historyMessages = [message0, message1]
  }

spec :: Spec
spec = do

  describe "historyOk" $ do
    it "should return initial value" $ do
      historyOk history `shouldBe` True

  describe "historyMessages" $ do
    it "should return initial value" $ do
      let messages = historyMessages history
      length messages `shouldBe` 2
      messages !! 0 `shouldBe` message0
      messages !! 1 `shouldBe` message1

  describe "parseHistory" $ do
    it "should return parsed from json" $ do
      let jsonPath = joinPath [takeDirectory __FILE__,"mock","History.json"]
      json <- readFile jsonPath
      parseHistory (pack json) `shouldBe` history
    it "should call error when parse error" $ do
      let json = pack "{\"okk\": true}"
      evaluate (parseHistory json) `shouldThrow` anyErrorCall
