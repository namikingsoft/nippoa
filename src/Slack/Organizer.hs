{-# LANGUAGE CPP #-}
module Slack.Organizer
  ( Organizer(..)
  , recordByMessage
  , channelByName
  ) where

import Text.Printf
  ( printf
  )
import Data.Maybe
  ( fromMaybe
  )
import Nippoa.Record
  ( Record(..)
  )
import Nippoa.Record.TimeStamp
  ( TimeStamp(..)
  , timeStampFromTs
  , timeStampToText
  )
import Nippoa.Record.Author
  ( Author(..)
  )
import Slack.UsersList
  ( UsersList(..)
  )
import Slack.GroupsList
  ( GroupsList(..)
  , fromGroupsName
  )
import Slack.Message
  ( Message(..)
  , toMarkdown
  )
import Slack.Channel
  ( Channel(..)
  )
import Slack.Attachment
  ( Attachment(..)
  )

data Organizer
  = Organizer
  { usersList :: UsersList
  , groupsList :: GroupsList
  }

recordByMessage :: Organizer -> Message -> Record
recordByMessage this x = case messageAttachments x of
  Just ys | attachesTitleLink ys /= "" ->
      Link
    { linkTimeStamp = timeStampFromTs . messageTs $ x
    , linkAuthor = Author . fromMaybe "" . messageUser $ x
    , linkText = attachesTitle ys
    , linkHref = attachesTitleLink ys
    }
  otherwise ->
      Plain
    { plainTimeStamp = timeStampFromTs . messageTs $ x
    , plainAuthor = Author . fromMaybe "" . messageUser $ x
    , plainText = toMarkdown . fromMaybe "" . messageText $ x
    }
  where
    attachesTitle = fromMaybe "" . attachmentTitle . head
    attachesTitleLink =  fromMaybe "" . attachmentTitleLink . head

channelByName :: Organizer -> String -> Maybe Channel
channelByName this name = fromGroupsName name $ groupsList this
