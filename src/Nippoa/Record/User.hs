{-# LANGUAGE CPP #-}
module Nippoa.Record.User
  ( User (..)
  ) where

data User = User
          { userId :: String
          } deriving (Show, Eq)
