module Slack.MessageSpec where

import Slack.Message
import Test.Hspec

message0 :: Message
message0 = Message
         { messageType = "message"
         , messageUser = Just "user0"
         , messageText = Just "text0"
         }

message1 :: Message
message1 = Message
         { messageType = "message"
         , messageUser = Just "user1"
         , messageText = Just "text1"
         }

spec :: Spec
spec = do

  describe "messageType" $ do
    it "should return initial value" $ do
      messageType message0 `shouldBe` "message"
      messageType message1 `shouldBe` "message"

  describe "user" $ do
    it "should return initial value" $ do
      messageUser message0 `shouldBe` Just "user0"
      messageUser message1 `shouldBe` Just "user1"

  describe "text" $ do
    it "should return initial value" $ do
      messageText message0 `shouldBe` Just "text0"
      messageText message1 `shouldBe` Just "text1"
