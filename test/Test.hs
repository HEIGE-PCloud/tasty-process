{- AUTOCOLLECT.TEST -}

module Test
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import System.Process (CreateProcess (..), StdStream (CreatePipe), proc)
import Test.Tasty.Process
import Test.Tasty.Providers (singleTest)

second :: Int
second = 1000000

test =
  singleTest
    "Simple test"
    TestProcess
      { process =
          (proc "test-executable-simple" [])
            { std_out = CreatePipe
            , std_err = CreatePipe
            , std_in = CreatePipe
            }
      , input = Nothing
      , exitCodeCheck = ignoreExitCode
      , stdoutCheck = equals "Hello, world!\n"
      , stderrCheck = ignoreOutput
      , timeout = 1 * second
      }

test =
  singleTest
    "Echo test"
    TestProcess
      { process =
          (proc "test-executable-echo" [])
            { std_out = CreatePipe
            , std_err = CreatePipe
            , std_in = CreatePipe
            }
      , input = Just "Echo!"
      , exitCodeCheck = ignoreExitCode
      , stdoutCheck = equals "Echo!"
      , stderrCheck = ignoreOutput
      , timeout = 1 * second
      }
