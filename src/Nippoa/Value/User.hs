{-# LANGUAGE CPP #-}
module Nippoa.Value.User
  ( User (..)
  ) where

data User = User
          { userId :: String
          } deriving (Show, Eq)
