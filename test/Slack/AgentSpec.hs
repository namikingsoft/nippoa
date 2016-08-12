{-# LANGUAGE CPP #-}
module Slack.AgentSpec where

import Slack.Agent()
import Test.Hspec

spec :: Spec
spec = do

  describe "getJsonFromUsersHistory" $ do
    it "should return json of users list" $ do
      pending

  describe "getJsonFromGroupsHistory" $ do
    it "should return json of groups list" $ do
      pending

  describe "getJsonFromChannelsHistory" $ do
    it "should return json of channels list" $ do
      pending

  describe "getJsonFromGroupsList" $ do
    it "should return json of groups history" $ do
      pending
