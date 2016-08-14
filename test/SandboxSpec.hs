{-# LANGUAGE CPP #-}
module SandboxSpec where

import Test.Hspec

import Text.Regex
import Codec.Binary.UTF8.String

example0 :: Maybe Bool
example0 = Just True

example1 :: [Int]
example1 = concat $
  [[1]]
  ++
  [[2], [3]]

example2 :: Int -> Maybe String
example2 x = show <$> (+3) <$> (*3) <$> Just x

example3 :: Maybe [Int]
example3 = map (+3) <$> Just [1, 2]

example4 :: Maybe [String]
example4 = map decodeString <$> (matchRegex regex . encodeString) "<test>"
  where
    regex = mkRegex "<(.*)>"

spec :: Spec
spec = do

  describe "example0" $ do
    it "should return maybe true" $ do
      example0 `shouldBe` Just True

  describe "example1" $ do
    it "should return maybe true" $ do
      example1 `shouldBe` [1, 2, 3]

  describe "example2" $ do
    it "should return maybe true" $ do
      example2 3 `shouldBe` Just "12"

  describe "example3" $ do
    it "should return maybe true" $ do
      example3 `shouldBe` Just [4, 5]

  describe "example4" $ do
    it "should return maybe true" $ do
      example4 `shouldBe` Just ["test"]
