{-# LANGUAGE CPP #-}
module Slack.OrganizerSpec where

import Slack.Organizer
import Test.Hspec

--import Nippoa.Record
--import Nippoa.Record.TimeStamp
--import Nippoa.Record.Author
import Slack.UsersListSpec
import Slack.GroupsListSpec
import Slack.ChannelsListSpec
import Slack.ChannelSpec
--import Slack.MessageSpec
--import Data.Maybe

organizer0 :: Organizer
organizer0 = Organizer
  { usersList = usersList0
  , groupsList = groupsList0
  , channelsList = channelsList0
  }

spec :: Spec
spec = do

  describe "recordsByMessage" $ do
    it "should create record by slack message" $ do
      pending
      -- let result = case recordByMessage organizer0 message0 of
      --       Link a b c d -> Just (a, b, c, d)
      --       _ -> Nothing
      -- let (time, author, text, href) = fromMaybe (error "") result
      -- authorId author `shouldBe` "user0"
      -- timeStampToText time `shouldBe` "2016-07-29 14:01:18"
      -- text `shouldBe` "title0"
      -- href `shouldBe` "titlelink0"

  describe "channelByName" $ do
    it "should return maybe channel by name" $ do
      channelByName organizer0 "general" `shouldBe` Just channel0
      channelByName organizer0 "random"  `shouldBe` Just channel1
      channelByName organizer0 "private" `shouldBe` Just channel2
      channelByName organizer0 "nothing" `shouldBe` Nothing
