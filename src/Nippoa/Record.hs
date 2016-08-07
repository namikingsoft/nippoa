{-# LANGUAGE CPP #-}
module Nippoa.Record
  ( Record(..)
  , createByMessage
  ) where

import Data.Maybe
  ( fromMaybe
  )
import Nippoa.Record.TimeStamp
  ( TimeStamp(..)
  , timeStampFromTs
  )
import Nippoa.Record.User
  ( User(..)
  )
import Slack.Message
  ( Message(..)
  )
import Slack.Attachment
  ( Attachment(..)
  )

data Record
  = Plain
  { plainTimeStamp :: TimeStamp
  , plainUser :: User
  , plainText :: String
  }
  | Link
  { linkTimeStamp :: TimeStamp
  , linkUser :: User
  , linkText :: String
  , linkHref :: String
  } deriving (Show, Eq)

createByMessage :: Message -> Record
createByMessage x
  | attachTitleLink x /= "" =
      Link
    { linkTimeStamp = timeStampFromTs . messageTs $ x
    , linkUser = User . fromMaybe "" . messageUser $ x
    , linkText = attachTitle $ x
    , linkHref = attachTitleLink $ x
    }
  | otherwise =
      Plain
    { plainTimeStamp = timeStampFromTs . messageTs $ x
    , plainUser = User . fromMaybe "" . messageUser $ x
    , plainText = fromMaybe "" . messageText $ x
    }
  where
    attachTitle = fromMaybe "" . attachmentTitle . attach
    attachTitleLink =  fromMaybe "" . attachmentTitleLink . attach
    attach = head . fromMaybe [] . messageAttachments
