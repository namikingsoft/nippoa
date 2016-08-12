{-# LANGUAGE CPP #-}
module Nippoa.RecordSpec where

import Nippoa.Record
import Test.Hspec

import Nippoa.Record.TimeStampSpec
import Nippoa.Record.AuthorSpec

plain :: Record
plain
  = Plain
  { plainTimeStamp = timestamp0
  , plainAuthor = author0
  , plainText = "text0"
  }

link :: Record
link
  = Link
  { linkTimeStamp = timestamp0
  , linkAuthor = author0
  , linkText = "text1"
  , linkHref = "href1"
  }

spec :: Spec
spec = do

  describe "plainTimeStamp" $ do
    it "should return initial value" $ do
      plainTimeStamp plain `shouldBe` timestamp0

  describe "plainAuthor" $ do
    it "should return initial value" $ do
      plainAuthor plain `shouldBe` author0

  describe "plainText" $ do
    it "should return initial value" $ do
      plainText plain `shouldBe` "text0"

  describe "linkTimeStamp" $ do
    it "should return initial value" $ do
      linkTimeStamp link `shouldBe` timestamp0

  describe "linkAuthor" $ do
    it "should return initial value" $ do
      linkAuthor link `shouldBe` author0

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
