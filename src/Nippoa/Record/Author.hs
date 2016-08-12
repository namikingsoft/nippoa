{-# LANGUAGE CPP #-}
module Nippoa.Record.Author
  ( Author (..)
  ) where

newtype Author
  = Author
  { authorId :: String
  } deriving (Show, Eq)
