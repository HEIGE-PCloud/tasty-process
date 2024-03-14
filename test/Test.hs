module Test (allTests) where

import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.Process

second :: Integer
second = 1000000

simpleTest :: TestTree
simpleTest =
  setTimeout (1 * second) $
    processTest
      "Simple test"
      TestProcess
        { process = proc "test-executable-simple" []
        , input = Nothing
        , stdoutCheck = equals "Hello, world!\n"
        , stderrCheck = equals "Hello, stderr!\n"
        , exitCodeCheck = equals ExitSuccess
        }

simpleShellTest :: TestTree
simpleShellTest =
  setTimeout (1 * second) $
    processTest
      "Simple shell test"
      TestProcess
        { process = shell "echo hello"
        , input = Nothing
        , stdoutCheck = equals "hello\n"
        , stderrCheck = equals ""
        , exitCodeCheck = equals ExitSuccess
        }

failedStdoutTest :: TestTree
failedStdoutTest =
  expectFailBecause "Expected failed" $
    processTest
      "Failed stdout test"
      defaultProcess
        { process = proc "test-executable-simple" []
        , stdoutCheck = equals "Hello world!\n"
        , stderrCheck = equals "Hello, stderr!\n"
        , exitCodeCheck = equals ExitSuccess
        }

failedStderrTest :: TestTree
failedStderrTest =
  expectFailBecause "Expected failed" $
    processTest
      "Failed stderr test"
      defaultProcess
        { process = proc "test-executable-simple" []
        , stdoutCheck = equals "Hello, world!\n"
        , stderrCheck = equals "Hello stderr!\n"
        , exitCodeCheck = equals ExitSuccess
        }

echoTest :: TestTree
echoTest =
  setTimeout (1 * second) $
    processTest
      "Echo test"
      defaultProcess
        { process = proc "test-executable-echo" []
        , input = Just "Echo!"
        , stdoutCheck = equals "Echo!"
        }

failedEchoTest :: TestTree
failedEchoTest =
  expectFailBecause "Expected '!ochE', but get 'Echo!'" $
    processTest
      "Failed echo test"
      defaultProcess
        { process = proc "test-executable-echo" []
        , input = Just "Echo!"
        , stdoutCheck = equals "!ochE"
        }

exitCodeTest :: TestTree
exitCodeTest =
  processTest
    "Exit code test"
    defaultProcess
      { process = proc "test-executable-exitcode" []
      , exitCodeCheck = equals (ExitFailure 123)
      }

failedExitCodeTest :: TestTree
failedExitCodeTest =
  expectFailBecause "Expected exit code 321, but get 123" $
    processTest
      "Failed exit code test"
      defaultProcess
        { process = proc "test-executable-exitcode" []
        , exitCodeCheck = equals (ExitFailure 321)
        }

timeoutTest :: TestTree
timeoutTest =
  expectFailBecause "Should timeout after 1s" $
    setTimeout (1 * second) $
      processTest
        "Timeout test"
        defaultProcess
          { process = proc "test-executable-sleep" []
          }

allTests :: TestTree
allTests =
  testGroup
    "Test"
    [ simpleTest
    , simpleShellTest
    , failedStdoutTest
    , failedStderrTest
    , echoTest
    , failedEchoTest
    , exitCodeTest
    , failedExitCodeTest
    , timeoutTest
    ]
