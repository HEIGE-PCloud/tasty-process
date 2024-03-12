{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Tasty.Process
  ( TestProcess (..)
  , ExitCodeCheck
  , OutputCheck
  , runTestProcess
  , ignoreOutput
  , ignoreExitCode
  , equals
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.DeepSeq (deepseq)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import GHC.IO.Handle (hClose, hFlush, hPutStr)
import System.Exit (ExitCode)
import System.IO (hGetContents)
import System.Process
  ( CreateProcess (CreateProcess, cmdspec)
  , createProcess
  , terminateProcess
  , waitForProcess
  )
import Test.Tasty.Providers (IsTest (..), Result, testFailed, testPassed)

type ExitCodeCheck = ExitCode -> Either String ()

type OutputCheck = String -> Either String ()

data TestProcess = TestProcess
  { process :: CreateProcess
  , input :: Maybe String
  , exitCodeCheck :: ExitCodeCheck
  , stdoutCheck :: OutputCheck
  , stderrCheck :: OutputCheck
  , timeout :: Int
  }

instance IsTest TestProcess where
  run _ p _ = runTestProcess p

  testOptions = return []

runTestProcess :: TestProcess -> IO Result
runTestProcess
  TestProcess
    { process
    , input
    , exitCodeCheck
    , stdoutCheck
    , stderrCheck
    , timeout
    } = do
    (mbStdinH, mbStdoutH, mbStderrH, ph) <- createProcess process
    for_ input $ \i -> do
      mapM_ (`hPutStr` i) mbStdinH
      mapM_ hFlush mbStdinH
      mapM_ hClose mbStdinH
    let
      processAction = do
        stdout :: String <- fromMaybe "" <$> mapM hGetContents mbStdoutH
        stderr :: String <- fromMaybe "" <$> mapM hGetContents mbStderrH
        exitCode :: ExitCode <- stderr `deepseq` stdout `deepseq` waitForProcess ph
        return (exitCode, stderr, stdout)
    result <- race (threadDelay timeout) processAction
    case result of
      Left () -> do
        terminateProcess ph
        exitCode <- waitForProcess ph
        return $ exitFailure process exitCode "" "" "process timeout"
      Right (exitCode, stderr, stdout) -> do
        let exitFailure' = exitFailure process exitCode stderr stdout
        let exitCodeCheckResult = exitCodeCheck exitCode
        let stderrCheckResult = stderrCheck stderr
        let stdoutCheckResult = stdoutCheck stdout
        let
          res
            | Left reason <- exitCodeCheckResult = exitFailure' reason
            | Left reason <- stderrCheckResult = exitFailure' reason
            | Left reason <- stdoutCheckResult = exitFailure' reason
            | otherwise = testPassed ""
        return res

exitFailure
  :: CreateProcess -> ExitCode -> String -> String -> String -> Result
exitFailure CreateProcess {cmdspec} code stderr stdout reason =
  testFailed $
    "process "
      ++ show cmdspec
      ++ " failed with code "
      ++ show code
      ++ "\nstderr was:\n"
      ++ stderr
      ++ "\nstdout was:\n"
      ++ stdout
      ++ "\nreason:\n"
      ++ reason

ignoreOutput :: OutputCheck
ignoreOutput _ = Right ()

ignoreExitCode :: ExitCodeCheck
ignoreExitCode _ = Right ()

class (Show a, Eq a) => EqualCheck a where
  equals :: a -> a -> Either String ()
  equals expected actual
    | expected == actual = Right ()
    | otherwise =
        Left $ "expected: " ++ show expected ++ "\nactual: " ++ show actual

instance EqualCheck String

instance EqualCheck ExitCode
