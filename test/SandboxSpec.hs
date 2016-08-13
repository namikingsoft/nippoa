{-# LANGUAGE CPP #-}
module SandboxSpec where

import Test.Hspec

example0 :: Maybe Bool
example0 = Just True

spec :: Spec
spec = do

  describe "example0" $ do
    it "should return maybe true" $ do
      example0 `shouldBe` Just True
