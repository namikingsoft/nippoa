{-# LANGUAGE CPP #-}
module Nippoa.Record
  ( Record(..)
  , recordByMessage
  , recordRender
  ) where

import Text.Printf
  ( printf
  )
import Data.Maybe
  ( fromMaybe
  )
import Nippoa.Record.TimeStamp
  ( TimeStamp(..)
  , timeStampFromTs
  , timeStampToText
  )
import Nippoa.Record.User
  ( User(..)
  )
import Slack.Message
  ( Message(..)
  , toMarkdown
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

recordByMessage :: Message -> Record
recordByMessage x = case messageAttachments x of
  Just ys | attachesTitleLink ys /= "" ->
      Link
    { linkTimeStamp = timeStampFromTs . messageTs $ x
    , linkUser = User . fromMaybe "" . messageUser $ x
    , linkText = attachesTitle ys
    , linkHref = attachesTitleLink ys
    }
  otherwise ->
      Plain
    { plainTimeStamp = timeStampFromTs . messageTs $ x
    , plainUser = User . fromMaybe "" . messageUser $ x
    , plainText = toMarkdown . fromMaybe "" . messageText $ x
    }
  where
    attachesTitle = fromMaybe "" . attachmentTitle . head
    attachesTitleLink =  fromMaybe "" . attachmentTitleLink . head

recordRender :: Record -> String
recordRender (Plain time user text) =
    printf "[%s] %s" (timeStampToText time) text
recordRender (Link time user text link) =
    printf "[%s] [%s](%s)" (timeStampToText time) text link
