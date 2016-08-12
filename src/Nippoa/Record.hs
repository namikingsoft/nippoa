{-# LANGUAGE CPP #-}
module Nippoa.Record
  ( Record(..)
  , recordRender
  ) where

import Text.Printf
  ( printf
  )
import Nippoa.Record.TimeStamp
  ( TimeStamp(..)
  , timeStampToText
  )
import Nippoa.Record.Author
  ( Author(..)
  )

data Record
  = Plain
  { plainTimeStamp :: TimeStamp
  , plainAuthor :: Author
  , plainText :: String
  }
  | Link
  { linkTimeStamp :: TimeStamp
  , linkAuthor :: Author
  , linkText :: String
  , linkHref :: String
  } deriving (Show, Eq)

recordRender :: Record -> String
recordRender (Plain time author text) =
    printf "[%s] %s" (timeStampToText time) text
recordRender (Link time author text link) =
    printf "[%s] [%s](%s)" (timeStampToText time) text link
