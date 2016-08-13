{-# LANGUAGE CPP #-}
module Nippoa.Record.Author
  ( Author (..)
  ) where

data Author
  = Author
  { authorName :: String
  , authorImage24 :: String
  , authorImage48 :: String
  }
  | None
  deriving (Show, Eq)
