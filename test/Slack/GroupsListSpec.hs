{-# LANGUAGE CPP #-}
module Slack.GroupsListSpec where

import Slack.GroupsList
import Test.Hspec

import System.FilePath.Posix
import Control.Exception (evaluate)
import Data.ByteString.Lazy.Char8 (pack)
import Slack.ChannelSpec

groupsList0 :: GroupsList
groupsList0
  = GroupsList
  { groupsListOk = True
  , groupsListGroups = [channel2]
  }

spec :: Spec
spec = do

  describe "groupsListOk" $ do
    it "should return initial value" $ do
      groupsListOk groupsList0 `shouldBe` True

  describe "groupsListGroups" $ do
    it "should return initial value" $ do
      let list = groupsListGroups groupsList0
      length list `shouldBe` 1
      list !! 0 `shouldBe` channel2

  describe "parseGroupsList" $ do
    it "should return parsed from json" $ do
      let jsonPath = joinPath [takeDirectory __FILE__,"mock","GroupsList.json"]
      json <- readFile jsonPath
      parseGroupsList (pack json) `shouldBe` groupsList0
    it "should call error when parse error" $ do
      let json = "{\"okk\": true}"
      evaluate (parseGroupsList $ pack json) `shouldThrow` anyErrorCall

  describe "channelByGroupsName" $ do
    it "should return maybe channel by name" $ do
      channelByGroupsName groupsList0 "private" `shouldBe` Just channel2
      channelByGroupsName groupsList0 "nothing" `shouldBe` Nothing
