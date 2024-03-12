{-# LANGUAGE NamedFieldPuns #-}

module Test.Tasty.Process where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.DeepSeq (deepseq)
import Data.Foldable (for_)
import GHC.IO.Handle (hClose, hFlush, hPutStr)
import System.Exit (ExitCode)
import System.IO (hGetContents)
import System.Process (runInteractiveProcess, terminateProcess, waitForProcess)
import Test.Tasty.Providers (IsTest (..), Result, testFailed, testPassed)

type ExitCodeCheck = ExitCode -> Either String ()
type OutputCheck = String -> Either String ()

data TestProcess
  = TestProcess
  { program :: String
  , arguments :: [String]
  , workingDir :: Maybe FilePath
  , environment :: Maybe [(String, String)]
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
    { program
    , arguments
    , workingDir
    , environment
    , input
    , exitCodeCheck
    , stdoutCheck
    , stderrCheck
    , timeout
    } = do
    (stdinH, stdoutH, stderrH, pid) <- runInteractiveProcess program arguments workingDir environment
    -- Write input to the process
    for_ input $ \i -> do
      hPutStr stdinH i
      hFlush stdinH
      hClose stdinH
    let
      processAction = do
        stderr <- hGetContents stderrH
        stdout <- hGetContents stdoutH
        ecode <- stderr `deepseq` stdout `deepseq` waitForProcess pid
        return (ecode, stderr, stdout)
    result <- race (threadDelay timeout) processAction
    case result of
      Left () -> do
        terminateProcess pid
        ecode <- waitForProcess pid
        return $ exitFailure program arguments ecode "" "" "program timeout"
      Right (ecode, stderr, stdout) -> do
        let exitFailure' = exitFailure program arguments ecode stderr stdout
        let exitCodeCheckResult = exitCodeCheck ecode
        let stderrCheckResult = stderrCheck stderr
        let stdoutCheckResult = stdoutCheck stdout
        let
          res
            | Left reason <- exitCodeCheckResult = exitFailure' reason
            | Left reason <- stderrCheckResult = exitFailure' reason
            | Left reason <- stdoutCheckResult = exitFailure' reason
            -- \| ecode /= exitCode =
            --     exitFailure'
            --       ("unexpected exit code " ++ show ecode ++ " expected " ++ show exitCode)
            -- \| not stderrCorrect = exitFailure' ("stderr is incorrect\n" ++ stderrReason)
            -- \| not stdoutCorrect = exitFailure' ("stdout is incorrect\n" ++ stdoutReason)
            | otherwise = testPassed ""
        return res

exitFailure ::
  String -> [String] -> ExitCode -> String -> String -> String -> Result
exitFailure file arguments code stderr stdout reason =
  testFailed $
    "program "
      ++ unwords (file : arguments)
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
