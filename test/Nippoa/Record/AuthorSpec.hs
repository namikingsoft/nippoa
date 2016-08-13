{-# LANGUAGE CPP #-}
module Nippoa.Record.AuthorSpec where

import Nippoa.Record.Author
import Test.Hspec

author0 :: Author
author0
  = Author
  { authorName = "name0"
  , authorImage24 = "http://image24"
  , authorImage48 = "http://image48"
  }

spec :: Spec
spec = do

  describe "authorName" $ do
    it "should return initial value" $ do
      authorName author0 `shouldBe` "name0"

  describe "authorImage24" $ do
    it "should return initial value" $ do
      authorImage24 author0 `shouldBe` "http://image24"

  describe "authorImage48" $ do
    it "should return initial value" $ do
      authorImage48 author0 `shouldBe` "http://image48"
