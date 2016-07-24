module Slack.ChannelSpec where

import Slack.Channel
import Test.Hspec
import Prelude hiding (id)

channel0 :: Channel
channel0 = Channel "id0" "general"

channel1 :: Channel
channel1 = Channel "id1" "random"

spec :: Spec
spec = do

  describe "id" $ do
    it "should return initial value" $ do
      id channel0 `shouldBe` "id0"
      id channel1 `shouldBe` "id1"

  describe "name" $ do
    it "should return initial value" $ do
      name channel0 `shouldBe` "general"
      name channel1 `shouldBe` "random"
