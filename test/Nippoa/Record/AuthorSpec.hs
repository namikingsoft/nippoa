{-# LANGUAGE CPP #-}
module Nippoa.Record.AuthorSpec where

import Nippoa.Record.Author
import Test.Hspec

author0 :: Author
author0 = Author "author0"

spec :: Spec
spec = do

  describe "authorId" $ do
    it "should return initial value" $ do
      authorId author0 `shouldBe` "author0"
