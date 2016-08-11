{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.UsersList
  ( UsersList(..)
  , parseUsersList
  , userById
  ) where

import Control.Applicative
  ( (<$>)
  , (<*>)
  )
import Data.Aeson
  ( Value(..)
  , FromJSON(..)
  , ToJSON(..)
  , object
  , decode
  , (.:)
  , (.=)
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.ByteString.Lazy.Internal
  ( ByteString
  )
import Slack.User
  ( User(..)
  )

data UsersList
  = UsersList
  { usersListOk :: Bool
  , usersListUsers :: [User]
  } deriving (Show, Eq)

instance FromJSON UsersList where
  parseJSON (Object v) = UsersList
    <$> v .: "ok"
    <*> v .: "members"

instance ToJSON UsersList where
  toJSON (UsersList ok users) = object
    [ "ok" .= ok
    , "members" .= users
    ]

parseUsersList :: ByteString -> UsersList
parseUsersList json = usersList
  where
    maybeUsersList = decode json :: Maybe UsersList
    usersList = fromMaybe (error "Parse Error") maybeUsersList

userById :: String -> UsersList -> Maybe User
userById id result
  | length hits > 0 = Just $ head hits
  | otherwise = Nothing
  where
    hits = filter (\x -> id == userId x) $ usersListUsers result
