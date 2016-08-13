{-# LANGUAGE CPP #-}
module Utility.BaseSpec where

import Utility.Base
import Test.Hspec

spec :: Spec
spec = do

  describe "liftJustList" $ do
    it "should return maybe true" $ do
      liftJustList (Just "0") `shouldBe` ["0"]
      liftJustList Nothing `shouldBe` ([] :: [String])

  describe "filterJust" $ do
    it "should return maybe true" $ do
      filterJust [Just "0", Nothing, Just "1"] `shouldBe` ["0", "1"]

  describe "isMaybe" $ do
    it "should return maybe true" $ do
      isMaybe (Just "0") `shouldBe` True
      isMaybe Nothing `shouldBe` False
