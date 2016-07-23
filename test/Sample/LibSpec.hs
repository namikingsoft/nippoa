module Sample.LibSpec where

import Sample.Lib
import Test.Hspec

spec :: Spec
spec = do

  describe "greeting" $ do
    it "should return greeting message" $ do
      greeting `shouldBe` "Hello World"
