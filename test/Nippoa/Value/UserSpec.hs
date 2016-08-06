{-# LANGUAGE CPP #-}
module Nippoa.Value.UserSpec where

import Nippoa.Value.User
import Test.Hspec

user0 :: User
user0 = User "user0"

spec :: Spec
spec = do

  describe "userId" $ do
    it "should return initial value" $ do
      userId user0 `shouldBe` "user0"
