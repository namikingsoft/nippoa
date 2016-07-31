{-# LANGUAGE CPP #-}
module Slack.AttachmentSpec where

import Slack.Attachment
import Test.Hspec

attachment0 :: Attachment
attachment0 = Attachment
            { attachmentFallback = "<http://example.com/>"
            , attachmentTitle = Just "title0"
            , attachmentText = Just "text0"
            }

attachment1 :: Attachment
attachment1 = Attachment
            { attachmentFallback = "<http://example.com/|Example>"
            , attachmentTitle = Just "title1"
            , attachmentText = Just "text1"
            }

spec :: Spec
spec = do

  describe "attachmentFallback" $ do
    it "should return initial value" $ do
      attachmentFallback attachment0 `shouldBe` "<http://example.com/>"
      attachmentFallback attachment1 `shouldBe` "<http://example.com/|Example>"

  describe "attachmentTitle" $ do
    it "should return initial value" $ do
      attachmentTitle attachment0 `shouldBe` Just "title0"
      attachmentTitle attachment1 `shouldBe` Just "title1"

  describe "attachmentText" $ do
    it "should return initial value" $ do
      attachmentText attachment0 `shouldBe` Just "text0"
      attachmentText attachment1 `shouldBe` Just "text1"

  describe "attachmentHeadline" $ do
    it "should return headline text" $ do
      attachmentHeadline attachment0 `shouldBe` "[http://example.com/](http://example.com/)"
      attachmentHeadline attachment1 `shouldBe` "[Example](http://example.com/)"
