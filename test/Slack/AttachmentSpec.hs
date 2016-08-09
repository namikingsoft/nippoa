{-# LANGUAGE CPP #-}
module Slack.AttachmentSpec where

import Slack.Attachment
import Test.Hspec

attachment0 :: Attachment
attachment0 = Attachment
            { attachmentFallback = "fallback0"
            , attachmentTitle = Just "title0"
            , attachmentTitleLink = Just "titlelink0"
            , attachmentText = Just "text0"
            , attachmentPreText = Just "pretext0"
            }

attachment1 :: Attachment
attachment1 = Attachment
            { attachmentFallback = "fallback1"
            , attachmentTitle = Just "title1"
            , attachmentTitleLink = Just "titlelink1"
            , attachmentText = Just "text1"
            , attachmentPreText = Just "pretext1"
            }

spec :: Spec
spec = do

  describe "attachmentFallback" $ do
    it "should return initial value" $ do
      attachmentFallback attachment0 `shouldBe` "fallback0"
      attachmentFallback attachment1 `shouldBe` "fallback1"

  describe "attachmentTitle" $ do
    it "should return initial value" $ do
      attachmentTitle attachment0 `shouldBe` Just "title0"
      attachmentTitle attachment1 `shouldBe` Just "title1"

  describe "attachmentTitleLink" $ do
    it "should return initial value" $ do
      attachmentTitleLink attachment0 `shouldBe` Just "titlelink0"
      attachmentTitleLink attachment1 `shouldBe` Just "titlelink1"

  describe "attachmentText" $ do
    it "should return initial value" $ do
      attachmentText attachment0 `shouldBe` Just "text0"
      attachmentText attachment1 `shouldBe` Just "text1"

  describe "attachmentPreText" $ do
    it "should return initial value" $ do
      attachmentPreText attachment0 `shouldBe` Just "pretext0"
      attachmentPreText attachment1 `shouldBe` Just "pretext1"
