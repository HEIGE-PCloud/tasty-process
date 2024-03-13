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
import Test.Tasty (TestName, TestTree, withResource)
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

instance IsTest TestProcess where
  run _ p _ = runTestProcess p

  testOptions = return []

instance
  IsTest
    (TestProcess, IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
  where
  run
    _
    ( TestProcess
        { process
        , exitCodeCheck
        , stdoutCheck
        , stderrCheck
        }
      , io
      )
    _ = do
      (_, mbStdoutH, mbStderrH, ph) <- io
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

  testOptions = return []

processTest
  :: TestName
  -> TestProcess
  -- -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  -- -> ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ())
  -- -> (IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> TestTree)
  -> TestTree
processTest
  testName
  tp =
    withResource
      (createResource tp)
      cleanupProcess
      (\a -> singleTest testName (tp, a))

createResource
  :: TestProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createResource TestProcess {process, input} = do
  r@(mbStdinH, mbStdoutH, mbStderrH, ph) <- createProcess process
  for_ input $ \i -> do
    mapM_ (`hPutStr` i) mbStdinH
    mapM_ hFlush mbStdinH
    mapM_ hClose mbStdinH
  return r

runTestProcess :: TestProcess -> IO Result
runTestProcess
  TestProcess
    { process
    , input
    , exitCodeCheck
    , stdoutCheck
    , stderrCheck
    } = do
    (mbStdinH, mbStdoutH, mbStderrH, ph) <- createProcess process
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
