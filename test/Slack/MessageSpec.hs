module Slack.MessageSpec where

import Slack.Message
import Test.Hspec

message0 :: Message
message0 = Message
         { messageType = "message"
         , messageUser = "user0"
         , messageText = "text0"
         }

message1 :: Message
message1 = Message
         { messageType = "message"
         , messageUser = "user1"
         , messageText = "text1"
         }

spec :: Spec
spec = do

  describe "messageType" $ do
    it "should return initial value" $ do
      messageType message0 `shouldBe` "message"
      messageType message1 `shouldBe` "message"

  describe "user" $ do
    it "should return initial value" $ do
      messageUser message0 `shouldBe` "user0"
      messageUser message1 `shouldBe` "user1"

  describe "text" $ do
    it "should return initial value" $ do
      messageText message0 `shouldBe` "text0"
      messageText message1 `shouldBe` "text1"
