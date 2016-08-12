{-# LANGUAGE CPP #-}
module Nippoa.RecordFactory
  ( recordByMessage
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
import Slack.Message
  ( Message(..)
  , toMarkdown
  )
import Slack.Attachment
  ( Attachment(..)
  )

recordByMessage :: Message -> Record
recordByMessage x = case messageAttachments x of
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
