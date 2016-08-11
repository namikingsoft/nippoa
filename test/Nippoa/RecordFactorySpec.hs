{-# LANGUAGE CPP #-}
module Nippoa.RecordFactorySpec where

import Nippoa.RecordFactory
import Test.Hspec

import Nippoa.Record
import Nippoa.Record.TimeStamp
import Nippoa.Record.User
import Slack.MessageSpec
import Data.Maybe

spec :: Spec
spec = do

  describe "recordByMessage" $ do
    it "should create record by slack message" $ do
      let result = case recordByMessage message0 of
            Link a b c d -> Just (a, b, c, d)
            _ -> Nothing
          (time, user, text, href) = fromMaybe (error "") result
      userId user `shouldBe` "user0"
      timeStampToText time `shouldBe` "2016-07-29 14:01:18"
      text `shouldBe` "title0"
      href `shouldBe` "titlelink0"
