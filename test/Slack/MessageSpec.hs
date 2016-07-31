{-# LANGUAGE CPP #-}
module Slack.MessageSpec where

import Slack.Message
import Test.Hspec
import Slack.AttachmentSpec

import Data.Time.LocalTime
import Data.Time.Format

message0 :: Message
message0 = Message
         { messageTs = "1469768478.000747"
         , messageType = "message"
         , messageUser = Just "user0"
         , messageText = Just "text0"
         , messageAttachments = Just [attachment0, attachment1]
         }

message1 :: Message
message1 = Message
         { messageTs = "1469768478.000747"
         , messageType = "message"
         , messageUser = Just "user1"
         , messageText = Just "text1"
         , messageAttachments = Just [attachment0, attachment1]
         }

spec :: Spec
spec = do

  describe "messageTs" $ do
    it "should return initial value" $ do
      messageTs message0 `shouldBe` "1469768478.000747"
      messageTs message1 `shouldBe` "1469768478.000747"

  describe "messageType" $ do
    it "should return initial value" $ do
      messageType message0 `shouldBe` "message"
      messageType message1 `shouldBe` "message"

  describe "messageUser" $ do
    it "should return initial value" $ do
      messageUser message0 `shouldBe` Just "user0"
      messageUser message1 `shouldBe` Just "user1"

  describe "messageText" $ do
    it "should return initial value" $ do
      messageText message0 `shouldBe` Just "text0"
      messageText message1 `shouldBe` Just "text1"

  describe "messageDateTime" $ do
    it "should return initial value" $ do
      let format = formatTime defaultTimeLocale "%F %T"
      format (messageDateTime message0) `shouldBe` "2016-07-29 14:01:18"
      format (messageDateTime message1) `shouldBe` "2016-07-29 14:01:18"

  describe "toMarkdown" $ do
    it "should return text to markdown" $ do
      let md1 = toMarkdown "<http://example.com/>"
          md2 = toMarkdown "<http://example.com/|Example>"
          md3 = toMarkdown "<http://example.com/|テスト>"
          md4 = toMarkdown "<http://example.com/|認証明>" -- fail Unicode '証'
          md5 = toMarkdown "<http://example.com/|あ　い>" -- fail Unicode '　'
      md1 `shouldBe` "[http://example.com/](http://example.com/)"
      md2 `shouldBe` "[Example](http://example.com/)"
      md3 `shouldBe` "[テスト](http://example.com/)"
      md4 `shouldBe` "[認証明](http://example.com/)"
      md5 `shouldBe` "[あ　い](http://example.com/)"
