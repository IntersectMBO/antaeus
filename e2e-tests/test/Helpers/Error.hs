{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: use this class for all errors
module Helpers.Error (
  Error (..),
  TimedOut (..),
) where

import System.Process.Internals (PHANDLE)

class Error e where
  displayError :: e -> String

instance Error () where
  displayError () = ""

data TimedOut = ProcessExitTimedOut Int PHANDLE deriving (Show)

instance Error TimedOut where
  displayError (ProcessExitTimedOut t pid) =
    "Timeout. Waited "
      ++ show t
      ++ "s in `cleanupTestnet` for process to exit. pid="
      ++ show pid
