module Test (allTests) where

import System.Exit (ExitCode (ExitFailure))
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
      defaultProcess
        { process = proc "test-executable-simple" []
        , stdoutCheck = equals "Hello, world!\n"
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
    , echoTest
    , failedEchoTest
    , exitCodeTest
    , failedExitCodeTest
    , timeoutTest
    ]
