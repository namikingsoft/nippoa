{-# LANGUAGE CPP #-}
module Nippoa.Record.User
  ( User (..)
  ) where

newtype User
  = User
  { userId :: String
  } deriving (Show, Eq)
