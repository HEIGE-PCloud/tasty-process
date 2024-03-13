{- AUTOCOLLECT.TEST -}

module Test
  (
  {- AUTOCOLLECT.TEST.export -}
  )
where

import System.Process (CreateProcess (..), StdStream (CreatePipe), proc)
import Test.Tasty.Process

second :: Integer
second = 1000000

test =
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

test =
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
