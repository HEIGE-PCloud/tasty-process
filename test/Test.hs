module Test (allTests) where

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

timeoutTest :: TestTree
timeoutTest =
  expectFailBecause "Should timeout after 1s" $
    setTimeout (1 * second) $
      processTest
        "Infinite loop test"
        defaultProcess
          { process = proc "test-executable-sleep" []
          }

allTests :: TestTree
allTests = testGroup "Test" [simpleTest, echoTest, timeoutTest]
