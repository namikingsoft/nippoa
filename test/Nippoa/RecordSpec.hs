{-# LANGUAGE CPP #-}
module Nippoa.RecordSpec where

import Nippoa.Record
import Test.Hspec

import Nippoa.Record.TimeStampSpec
import Nippoa.Record.UserSpec

plain :: Record
plain
  = Plain
  { plainTimeStamp = timestamp0
  , plainUser = user0
  , plainText = "text0"
  }

link :: Record
link
  = Link
  { linkTimeStamp = timestamp0
  , linkUser = user0
  , linkText = "text1"
  , linkHref = "href1"
  }

spec :: Spec
spec = do

  describe "plainTimeStamp" $ do
    it "should return initial value" $ do
      plainTimeStamp plain `shouldBe` timestamp0

  describe "plainUser" $ do
    it "should return initial value" $ do
      plainUser plain `shouldBe` user0

  describe "plainText" $ do
    it "should return initial value" $ do
      plainText plain `shouldBe` "text0"

  describe "linkTimeStamp" $ do
    it "should return initial value" $ do
      linkTimeStamp link `shouldBe` timestamp0

  describe "linkUser" $ do
    it "should return initial value" $ do
      linkUser link `shouldBe` user0

  describe "linkText" $ do
    it "should return initial value" $ do
      linkText link `shouldBe` "text1"

  describe "linkHref" $ do
    it "should return initial value" $ do
      linkHref link `shouldBe` "href1"

  describe "recordRender" $ do
    it "should return text of rendered record" $ do
      recordRender plain `shouldBe` "[2016-07-29 14:01:18] text0"
      recordRender link `shouldBe` "[2016-07-29 14:01:18] [text1](href1)"
