{-# LANGUAGE CPP #-}
module Slack.GroupsListSpec where

import Slack.GroupsList
import Slack.Channel
import Test.Hspec

import System.FilePath.Posix
import Control.Exception (evaluate)
import Data.ByteString.Lazy.Char8 (pack)
import Slack.ChannelSpec

groupsList :: GroupsList
groupsList = GroupsList
           { groupsListOk = True
           , groupsListGroups = [channel0, channel1]
           }

spec :: Spec
spec = do

  describe "groupsListOk" $ do
    it "should return initial value" $ do
      groupsListOk groupsList `shouldBe` True

  describe "groupsListGroups" $ do
    it "should return initial value" $ do
      let list = groupsListGroups groupsList
      length list `shouldBe` 2
      list !! 0 `shouldBe` channel0
      list !! 1 `shouldBe` channel1

  describe "parseGroupsList" $ do
    it "should return parsed from json" $ do
      let jsonPath = joinPath [takeDirectory __FILE__,"mock","GroupsList.json"]
      json <- readFile jsonPath
      parseGroupsList (pack json) `shouldBe` groupsList
    it "should call error when parse error" $ do
      let json = "{\"okk\": true}"
      evaluate (parseGroupsList $ pack json) `shouldThrow` anyErrorCall

  describe "fromGroupsName" $ do
    it "should return maybe channel" $ do
      fromGroupsName groupsList "general" `shouldBe` Just channel0
      fromGroupsName groupsList "random"  `shouldBe` Just channel1
      fromGroupsName groupsList "nothing" `shouldBe` Nothing
