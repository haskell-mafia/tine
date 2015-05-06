{-# LANGUAGE NoImplicitPrelude #-}
module Tine.Process (
    ProcessOrPin (..)
  , execProcess
  , execProcessOrPin
  , execProcessOrTerminateOnPin
  , execProcessOrTerminateGroupOnPin
  , waitForProcessOrPin
  , exitLeft
  , signalToExit
  , terminateGroup
  , terminate
  , stopGroup
  , stop
  , module System.Process
  ) where

import           P

import           System.Exit
import           System.IO
import           System.Process
import           System.Process.Internals
import           System.Posix.Process
import           System.Posix.Signals
import           System.Posix.Types

import           Tine.Data
import           Tine.Snooze


data ProcessOrPin =
  ProcessStopped ExitCode | PinPulled ProcessHandle

execProcess :: CreateProcess -> IO ExitCode
execProcess p =
  fmap pickHandle (createProcess p) >>= waitForProcess

execProcessOrPin :: Pin -> CreateProcess -> IO ProcessOrPin
execProcessOrPin pin p =
  fmap pickHandle (createProcess p) >>= waitForProcessOrPin pin

execProcessOrTerminateOnPin :: Pin -> CreateProcess -> IO ExitCode
execProcessOrTerminateOnPin pin p =
  execProcessOrPin pin p >>= \pp -> case pp of
    ProcessStopped e ->
      pure e
    PinPulled h ->
      terminate h

execProcessOrTerminateGroupOnPin :: Pin -> CreateProcess -> IO ExitCode
execProcessOrTerminateGroupOnPin pin p =
  execProcessOrPin pin p { create_group = True } >>= \pp -> case pp of
    ProcessStopped e ->
      pure e
    PinPulled h ->
      terminateGroup h

waitForProcessOrPin :: Pin -> ProcessHandle -> IO ProcessOrPin
waitForProcessOrPin pin h =
  getProcessExitCode h >>= \e' -> case e' of
    Just e ->
      pure . ProcessStopped $ e
    Nothing ->
      ifM (checkPin pin) (pure . PinPulled $ h) $
        snooze (milliseconds 100) >> waitForProcessOrPin pin h

exitLeft :: ExitCode -> Either ExitCode ()
exitLeft r = case r of
  ExitSuccess -> Right ()
  ExitFailure _ -> Left r

signalToExit :: Signal -> ExitCode
signalToExit =
  ExitFailure . negate . fromInteger . toInteger

terminateGroup :: ProcessHandle -> IO ExitCode
terminateGroup h =
  withProcessHandle h $ \(OpenHandle pid') ->
    getProcessGroupIDOf pid' >>= stopGroup pid' >>= pure . signalToExit

terminate :: ProcessHandle -> IO ExitCode
terminate h =
  withProcessHandle h $ \(OpenHandle pid') ->
    stop pid' >>= pure . signalToExit

stopGroup :: ProcessID -> ProcessGroupID -> IO Signal
stopGroup pid' pgid' =
  stopWith (flip signalProcessGroup pgid') pid'

stop :: ProcessID -> IO Signal
stop pid' =
  stopWith (flip signalProcess pid') pid'

stopWith :: (Signal -> IO ()) -> ProcessID -> IO Signal
stopWith sig pid' = go (5 :: Int)
  where go 0 =
          sigKILL <$ sig sigKILL
        go n =
          check $ do
            sig sigTERM
            check $ do
              snooze . seconds $ 1
              check $
                go $ n - 1
        check f = do
          st <- getProcessStatus False True pid'
          case st of
            Nothing -> f
            Just _ -> pure sigTERM

pickHandle :: (a, b, c, d) -> d
pickHandle (_, _, _, d) =
  d
