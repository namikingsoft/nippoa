{-# LANGUAGE CPP #-}
module Slack.OrganizerSpec where

import Slack.Organizer
import Test.Hspec

import Slack.UsersListSpec
import Slack.GroupsListSpec
import Slack.ChannelsListSpec
import Slack.ChannelSpec

organizer0 :: Organizer
organizer0
  = Organizer
  { organizerUsersList = usersList0
  , organizerGroupsList = groupsList0
  , organizerChannelsList = channelsList0
  }

spec :: Spec
spec = do

  describe "organizerUsersList" $ do
    it "should create organizer" $ do
      organizerUsersList organizer0 `shouldBe` usersList0

  describe "organizerGroupsList" $ do
    it "should create organizer" $ do
      organizerGroupsList organizer0 `shouldBe` groupsList0

  describe "organizerChannelsList" $ do
    it "should create organizer" $ do
      organizerChannelsList organizer0 `shouldBe` channelsList0

  describe "getOrganizer" $ do
    it "should create organizer" $ do
      pending

  describe "recordsByMessage" $ do
    it "should create record by slack message" $ do
      pending

  describe "channelByName" $ do
    it "should return maybe channel by name" $ do
      channelByName organizer0 "general" `shouldBe` Just channel0
      channelByName organizer0 "random"  `shouldBe` Just channel1
      channelByName organizer0 "private" `shouldBe` Just channel2
      channelByName organizer0 "nothing" `shouldBe` Nothing
