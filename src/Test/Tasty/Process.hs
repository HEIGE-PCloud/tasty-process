{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Tasty.Process
  ( TestProcess (..)
  , ExitCodeCheck
  , OutputCheck
  , EqualCheck (..)
  , IgnoreCheck (..)
  , processTest
  , setTimeout
  )
where

import Control.DeepSeq (deepseq)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import GHC.IO.Handle (Handle, hClose, hFlush, hPutStr)
import System.Exit (ExitCode)
import System.IO (hGetContents)
import System.Process
  ( CreateProcess (CreateProcess, cmdspec)
  , ProcessHandle
  , cleanupProcess
  , createProcess
  , waitForProcess
  )
import Test.Tasty (TestName, TestTree, adjustOption, mkTimeout, withResource)
import Test.Tasty.Providers
  ( IsTest (..)
  , Result
  , singleTest
  , testFailed
  , testPassed
  )

type ExitCodeCheck = ExitCode -> Either String ()

type OutputCheck = String -> Either String ()

data TestProcess = TestProcess
  { process :: CreateProcess
  , input :: Maybe String
  , exitCodeCheck :: ExitCodeCheck
  , stdoutCheck :: OutputCheck
  , stderrCheck :: OutputCheck
  }

instance
  IsTest
    (TestProcess, IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
  where
  run _ (tp, io) _ = runTestProcess tp io

  testOptions = return []

processTest
  :: TestName
  -> TestProcess
  -> TestTree
processTest
  testName
  tp@TestProcess {process} =
    withResource
      (createProcess process)
      cleanupProcess
      (\io -> singleTest testName (tp, io))

runTestProcess
  :: TestProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  -> IO Result
runTestProcess
  TestProcess
    { process
    , input
    , exitCodeCheck
    , stdoutCheck
    , stderrCheck
    }
  io = do
    (mbStdinH, mbStdoutH, mbStderrH, ph) <- io
    for_ input $ \i -> do
      mapM_ (`hPutStr` i) mbStdinH
      mapM_ hFlush mbStdinH
      mapM_ hClose mbStdinH
    stdout :: String <- fromMaybe "" <$> mapM hGetContents mbStdoutH
    stderr :: String <- fromMaybe "" <$> mapM hGetContents mbStderrH
    exitCode :: ExitCode <- stderr `deepseq` stdout `deepseq` waitForProcess ph
    let exitFailure' = exitFailure process exitCode stderr stdout
    let exitCodeCheckResult = exitCodeCheck exitCode
    let stderrCheckResult = stderrCheck stderr
    let stdoutCheckResult = stdoutCheck stdout
    let res
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

setTimeout :: Integer -> TestTree -> TestTree
setTimeout x = adjustOption (const (mkTimeout x))

class (Show a, Eq a) => EqualCheck a where
  equals :: a -> a -> Either String ()
  equals expected actual
    | expected == actual = Right ()
    | otherwise =
        Left $ "expected: " ++ show expected ++ "\nactual: " ++ show actual

instance EqualCheck String

instance EqualCheck ExitCode

class IgnoreCheck a where
  ignored :: a -> Either String ()
  ignored _ = Right ()

instance IgnoreCheck String

instance IgnoreCheck ExitCode
