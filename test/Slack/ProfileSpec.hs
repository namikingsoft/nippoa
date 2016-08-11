{-# LANGUAGE CPP #-}
module Slack.ProfileSpec where

import Slack.Profile
import Test.Hspec

profile0 :: Profile
profile0
  = Profile
  { profileRealName = "realname0"
  , profileImage24 = "image240"
  , profileImage48 = "image480"
  , profileBotId = Just "botid0"
  }

profile1 :: Profile
profile1
  = Profile
  { profileRealName = "realname1"
  , profileImage24 = "image241"
  , profileImage48 = "image481"
  , profileBotId = Just "botid1"
  }

spec :: Spec
spec = do

  describe "profileRealName" $ do
    it "should return initial value" $ do
      profileRealName profile0 `shouldBe` "realname0"
      profileRealName profile1 `shouldBe` "realname1"

  describe "profileImage24" $ do
    it "should return initial value" $ do
      profileImage24 profile0 `shouldBe` "image240"
      profileImage24 profile1 `shouldBe` "image241"

  describe "profileImage48" $ do
    it "should return initial value" $ do
      profileImage48 profile0 `shouldBe` "image480"
      profileImage48 profile1 `shouldBe` "image481"

  describe "profileBotId" $ do
    it "should return initial value" $ do
      profileBotId profile0 `shouldBe` Just "botid0"
      profileBotId profile1 `shouldBe` Just "botid1"
