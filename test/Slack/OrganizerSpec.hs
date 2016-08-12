{-# LANGUAGE CPP #-}
module Slack.OrganizerSpec where

import Slack.Organizer
import Test.Hspec

import Nippoa.Record
import Nippoa.Record.TimeStamp
import Nippoa.Record.Author
import Slack.UsersListSpec
import Slack.GroupsListSpec
import Slack.MessageSpec
import Data.Maybe

organizer0 :: Organizer
organizer0 = Organizer
  { usersList = usersList0
  , groupsList = groupsList0
  }

spec :: Spec
spec = do

  describe "recordByMessage" $ do
    it "should create record by slack message" $ do
      let result = case recordByMessage organizer0 message0 of
            Link a b c d -> Just (a, b, c, d)
            _ -> Nothing
      let (time, author, text, href) = fromMaybe (error "") result
      authorId author `shouldBe` "user0"
      timeStampToText time `shouldBe` "2016-07-29 14:01:18"
      text `shouldBe` "title0"
      href `shouldBe` "titlelink0"
