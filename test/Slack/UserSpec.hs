{-# LANGUAGE CPP #-}
module Slack.UserSpec where

import Slack.User
import Test.Hspec

import Slack.ProfileSpec

user0 :: User
user0
  = User
  { userId = "id0"
  , userName = "user0"
  , userProfile = profile0
  }

user1 :: User
user1
  = User
  { userId = "id1"
  , userName = "user1"
  , userProfile = profile1
  }

spec :: Spec
spec = do

  describe "userId" $ do
    it "should return initial value" $ do
      userId user0 `shouldBe` "id0"
      userId user1 `shouldBe` "id1"

  describe "userName" $ do
    it "should return initial value" $ do
      userName user0 `shouldBe` "user0"
      userName user1 `shouldBe` "user1"

  describe "userProfile" $ do
    it "should return initial value" $ do
      userProfile user0 `shouldBe` profile0
      userProfile user1 `shouldBe` profile1
