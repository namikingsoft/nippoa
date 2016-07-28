module Slack.AttachmentSpec where

import Slack.Attachment
import Test.Hspec

attachment0 :: Attachment
attachment0 = Attachment
         { attachmentFallback = "fallback0"
         , attachmentTitle = Just "title0"
         , attachmentText = Just "text0"
         }

attachment1 :: Attachment
attachment1 = Attachment
         { attachmentFallback = "fallback1"
         , attachmentTitle = Just "user1"
         , attachmentText = Just "text1"
         }

spec :: Spec
spec = do

  describe "attachmentFallback" $ do
    it "should return initial value" $ do
      attachmentFallback attachment0 `shouldBe` "fallback0"
      attachmentFallback attachment1 `shouldBe` "fallback1"

  describe "user" $ do
    it "should return initial value" $ do
      attachmentTitle attachment0 `shouldBe` Just "title0"
      attachmentTitle attachment1 `shouldBe` Just "user1"

  describe "text" $ do
    it "should return initial value" $ do
      attachmentText attachment0 `shouldBe` Just "text0"
      attachmentText attachment1 `shouldBe` Just "text1"
