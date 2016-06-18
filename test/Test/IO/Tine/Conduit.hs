{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Tine.Conduit where

import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Disorder.Corpus
import           Disorder.Core.IO

import           P

import           System.Exit (ExitCode (..))
import           System.IO (IO)
import           System.Process (proc, shell)
import           System.Posix.Signals

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tine.Conduit (Out)
import qualified Tine.Conduit as X
import           Tine.Data
import           Tine.Process (signalToExit)


prop_capture = forAll (elements viruses) $ \t ->
  let
    o = CL.sinkNull
    e = CL.sinkNull
  in testIO $ do
    (ro, re, s) <- X.capture o e (proc "echo" [T.unpack $ t])
    pure $ (ro, re, s) === (T.encodeUtf8 t <> "\n", "", ExitSuccess)

prop_exec =
  let
    o = CL.sinkNull
    e = CL.sinkNull
  in testIO $ do
    t <- X.exec o e (proc "true" [])
    f <- X.exec o e (proc "false" [])
    pure $ (t, f) === (ExitSuccess, ExitFailure 1)

prop_raw = forAll (elements viruses) $ \t ->
  let
    o = CL.consume
    e = CL.consume
  in testIO $ do
    (ro, re, s) <- X.raw o e (proc "echo" [T.unpack $ t])
    pure $ (mconcat ro, mconcat re, s) === (T.encodeUtf8 t <> "\n", "", ExitSuccess)

prop_execOrTerminateOnPin_pulled = once . testIO . withPulledPin  $ \pin -> do
  r <- X.execOrTerminateOnPin pin nullout nullout . shell $ "sleep 1"
  pure $ r === (X.StreamingPinPulled $ signalToExit sigTERM)

prop_execOrTerminateOnPin_not_pulled = once . testIO . withPin  $ \pin -> do
  r <- X.execOrTerminateOnPin pin nullout nullout . shell $ "sleep 1"
  pure $ r === (X.StreamingProcessStopped ExitSuccess)

withPin :: (Pin -> IO a) -> IO a
withPin f =
  newPin >>= f

withPulledPin :: (Pin -> IO a) -> IO a
withPulledPin f =
  newPin >>= \p -> pullPin p >> f p

nullout :: Out
nullout =
  CL.sinkNull

return []
tests = $quickCheckAll
