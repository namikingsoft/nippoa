{-# LANGUAGE CPP #-}
module Utility.Base
  ( liftJustList
  , filterJust
  , isMaybe
  ) where

import Data.Maybe
  ( fromMaybe
  )

liftJustList :: Maybe a -> [a]
liftJustList (Just n) = [n]
liftJustList Nothing = []

filterJust :: [Maybe a] -> [a]
filterJust = map fromJust . filter isMaybe
  where
    fromJust = fromMaybe $ error "Impossible Error"

isMaybe :: Maybe a -> Bool
isMaybe (Just n) = True
isMaybe Nothing = False
