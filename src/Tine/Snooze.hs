{-# LANGUAGE NoImplicitPrelude #-}
module Tine.Snooze (
    snooze
  , module Tine.Data.Duration
  ) where

import           Control.Concurrent

import           P

import           System.IO

import           Tine.Data.Duration


snooze :: Duration -> IO ()
snooze =
  threadDelay . toMicroseconds
