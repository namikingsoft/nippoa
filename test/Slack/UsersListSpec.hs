{-# LANGUAGE CPP #-}
module Slack.UsersListSpec where

import Slack.UsersList
import Test.Hspec

import System.FilePath.Posix
import Control.Exception (evaluate)
import Data.ByteString.Lazy.Char8 (pack)
import Slack.UserSpec

usersList :: UsersList
usersList
  = UsersList
  { usersListOk = True
  , usersListUsers = [user0, user1]
  }

spec :: Spec
spec = do

  describe "usersListOk" $ do
    it "should return initial value" $ do
      usersListOk usersList `shouldBe` True

  describe "usersListUsers" $ do
    it "should return initial value" $ do
      let list = usersListUsers usersList
      length list `shouldBe` 2
      list !! 0 `shouldBe` user0
      list !! 1 `shouldBe` user1

  describe "parseUsersList" $ do
    it "should return parsed from json" $ do
      let jsonPath = joinPath [takeDirectory __FILE__,"mock","UsersList.json"]
      json <- readFile jsonPath
      parseUsersList (pack json) `shouldBe` usersList
    it "should call error when parse error" $ do
      let json = "{\"okk\": true}"
      evaluate (parseUsersList $ pack json) `shouldThrow` anyErrorCall

  describe "userById" $ do
    it "should return maybe user by id" $ do
      userById "id0" usersList `shouldBe` Just user0
      userById "id1" usersList `shouldBe` Just user1
      userById "nothing" usersList `shouldBe` Nothing
