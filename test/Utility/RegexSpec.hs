{-# LANGUAGE CPP #-}
module Utility.RegexSpec where

import Utility.Regex
import Test.Hspec

spec :: Spec
spec = do

  describe "match" $ do
    it "should match using regex" $ do
      match "<(.*)>" "<hello>" `shouldBe` Just ["hello"]
      match "<.*>" "<hello>" `shouldBe` Just []
      match "<.*>" "[hello]" `shouldBe` Nothing

  describe "split" $ do
    it "should split using regex" $ do
      split "," "hello,world" `shouldBe` ["hello", "world"]
      split "," "hello world" `shouldBe` ["hello world"]

  describe "replace" $ do
    it "should replace using regex" $ do
      replace "<(.*)>" "\\1" "<hello>" `shouldBe` "hello"
