{-# LANGUAGE CPP #-}
module Utility.Regex
  ( match
  , split
  , replace
  ) where

import Text.Regex
  ( mkRegex
  , matchRegex
  , splitRegex
  , subRegex
  )
import Codec.Binary.UTF8.String
  ( encodeString
  , decodeString
  )

match :: String -> String -> Maybe [String]
match regtext source =
    map decodeString <$> (matchRegex regex . encodeString) source
  where
    regex = mkRegex regtext

split :: String -> String -> [String]
split regtext =
    map decodeString . splitRegex regex . encodeString
  where
    regex = mkRegex regtext

replace :: String -> String -> String -> String
replace regtext reptext source =
    decodeString . subRegex regex (encodeString source) $ reptext
  where
    regex = mkRegex regtext
