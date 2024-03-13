{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Tasty.Process
  ( processTest
  , TestProcess (..)
  , ExitCodeCheck
  , OutputCheck
  , EqualCheck (..)
  , IgnoreCheck (..)
  , setTimeout
  , proc
  , shell
  , defaultProcess
  )
where

import Control.DeepSeq (deepseq)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import GHC.IO.Handle (Handle, hClose, hFlush, hPutStr)
import System.Exit (ExitCode)
import System.IO (hGetContents)
import System.Process
  ( CmdSpec (..)
  , CreateProcess (..)
  , ProcessHandle
  , StdStream (..)
  , cleanupProcess
  , createProcess
  , waitForProcess
  )
import qualified System.Process as P (proc, shell)
import Test.Tasty (TestName, TestTree, localOption, mkTimeout, withResource)
import Test.Tasty.Providers
  ( IsTest (..)
  , Result
  , singleTest
  , testFailed
  , testPassed
  )

{- | Create a 'TestTree' from a 'TestProcess'. Here is an example of how to use
the function to create a test.

@
exampleTest :: TestTree
exampleTest =
  setTimeout (1 * second) $
    processTest
      "Simple test"
      TestProcess
        { process =
          (proc "test-executable-simple" [])
        , input = Nothing
        , exitCodeCheck = ignored
        , stdoutCheck = equals "Hello, world!\n"
        , stderrCheck = ignored
        }
@
-}
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

{- | 'ExitCodeCheck' is a function that given the 'ExitCode' of a process,
returns '()' if the exit code is expected, or a reason otherwise.
-}
type ExitCodeCheck = ExitCode -> Either String ()

{- | 'OutputCheck' is a function that given the output of a process,
returns '()' if the output is expected, or a reason otherwise.
-}
type OutputCheck = String -> Either String ()

-- | 'TestProcess' is a data type that represents a process to be tested.
data TestProcess = TestProcess
  { process :: CreateProcess
  -- ^ The process to be tested.
  , input :: Maybe String
  -- ^ The input to be sent to the process. If 'Nothing', no input will be sent.
  , exitCodeCheck :: ExitCodeCheck
  -- ^ The check to be performed on the exit code of the process.
  , stdoutCheck :: OutputCheck
  -- ^ The check to be performed on the @stdout@ of the process.
  , stderrCheck :: OutputCheck
  -- ^ The check to be performed on the @stderr@ of the process.
  }

instance
  IsTest
    (TestProcess, IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
  where
  run _ (tp, io) _ = runTestProcess tp io

  testOptions = return []

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
          | Left reason <- exitCodeCheckResult =
              exitFailure' ("ExitCode check failed.\n" ++ reason)
          | Left reason <- stdoutCheckResult =
              exitFailure' ("Stdout check failed.\n" ++ reason)
          | Left reason <- stderrCheckResult =
              exitFailure' ("Stderr check failed.\n" ++ reason)
          | otherwise = testPassed ""
    return res

exitFailure
  :: CreateProcess -> ExitCode -> String -> String -> String -> Result
exitFailure CreateProcess {cmdspec} code stderr stdout reason =
  testFailed $
    unlines
      [ printCmdSpec cmdspec ++ " exited with code " ++ show code
      , ""
      , if null stdout
          then "Nothing was printed to stdout."
          else "stdout contained:\n" ++ stdout
      , ""
      , if null stderr
          then "Nothing was printed to stderr."
          else "stderr contained:\n" ++ stderr
      , ""
      , reason
      ]

printCmdSpec :: CmdSpec -> String
printCmdSpec (ShellCommand x) = x
printCmdSpec (RawCommand x y) = unwords (x : y)

-- | Set the timeout for a 'TestTree'.
setTimeout :: Integer -> TestTree -> TestTree
setTimeout = localOption . mkTimeout

class (Show a, Eq a) => EqualCheck a where
  equals :: a -> a -> Either String ()
  equals expected actual
    | expected == actual = Right ()
    | otherwise =
        Left $ "expected : " ++ show expected ++ "\nactual   : " ++ show actual

instance EqualCheck String

instance EqualCheck ExitCode

class IgnoreCheck a where
  ignored :: a -> Either String ()
  ignored _ = Right ()

instance IgnoreCheck String

instance IgnoreCheck ExitCode

-- | Re-export of 'proc' from "System.Process" for more convenient default values.
proc :: FilePath -> [String] -> CreateProcess
proc x y =
  (P.proc x y) {std_out = CreatePipe, std_err = CreatePipe, std_in = CreatePipe}

-- | Re-export of 'shell' from "System.Process" for more convenient default values.
shell :: String -> CreateProcess
shell x = (P.shell x) {std_out = CreatePipe, std_err = CreatePipe, std_in = CreatePipe}

defaultProcess :: TestProcess
defaultProcess =
  TestProcess
    { process = undefined
    , input = Nothing
    , exitCodeCheck = ignored
    , stdoutCheck = ignored
    , stderrCheck = ignored
    }
