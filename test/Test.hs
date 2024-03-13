module Test (allTests) where

import System.Process (CreateProcess (..), StdStream (CreatePipe))
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
        { process =
            (proc "test-executable-simple" [])
              { std_out = CreatePipe
              , std_err = CreatePipe
              , std_in = CreatePipe
              }
        , input = Nothing
        , exitCodeCheck = ignored
        , stdoutCheck = equals "Hello, world!\n"
        , stderrCheck = ignored
        }

echoTest :: TestTree
echoTest =
  setTimeout (1 * second) $
    processTest
      "Echo test"
      TestProcess
        { process =
            (proc "test-executable-echo" [])
              { std_out = CreatePipe
              , std_err = CreatePipe
              , std_in = CreatePipe
              }
        , input = Just "Echo!"
        , exitCodeCheck = ignored
        , stdoutCheck = equals "Echo!"
        , stderrCheck = ignored
        }

timeoutTest :: TestTree
timeoutTest =
  expectFailBecause "Should timeout after 1s" $
    setTimeout (1 * second) $
      processTest
        "Infinite loop test"
        TestProcess
          { process =
              (proc "test-executable-sleep" [])
                { std_out = CreatePipe
                , std_err = CreatePipe
                , std_in = CreatePipe
                }
          , input = Nothing
          , exitCodeCheck = ignored
          , stdoutCheck = ignored
          , stderrCheck = ignored
          }

allTests :: TestTree
allTests = testGroup "Test" [simpleTest, echoTest, timeoutTest]
