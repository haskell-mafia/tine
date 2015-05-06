{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Tine.Process where

import           P

import           System.Exit
import           System.IO
import           System.Posix.Signals

import           Test.QuickCheck

import           Tine.Process

prop_exitLeft_ok =
  exitLeft ExitSuccess === Right ()

prop_exitLeft_err =
  forAll genExitFailure $ \e ->
    exitLeft e === Left e

prop_signalToExit =
  forAll (elements [sigTERM, sigQUIT, sigINT, sigHUP]) $ \s ->
    case signalToExit s of
      ExitSuccess ->
        counterexample "Didn't expect a signal to be a success." False
      ExitFailure n ->
        negate n === (fromInteger . toInteger) s

prop_processOrPin_stopped n m =
  forAll genExitCode $ \e ->
    processOrPin (const (n :: Int)) (const m) (ProcessStopped e) === n

genExitFailure =
  ExitFailure <$> choose (1, 125)

genExitCode =
  oneof [pure ExitSuccess, genExitFailure]

return []
tests :: IO Bool
tests = $quickCheckAll
