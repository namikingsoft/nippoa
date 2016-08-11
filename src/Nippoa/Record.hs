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
import Nippoa.Record.User
  ( User(..)
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

recordRender :: Record -> String
recordRender (Plain time user text) =
    printf "[%s] %s" (timeStampToText time) text
recordRender (Link time user text link) =
    printf "[%s] [%s](%s)" (timeStampToText time) text link
