{- AUTOCOLLECT.TEST -}

module Test (
{- AUTOCOLLECT.TEST.export -}

) where

import Test.Tasty.Process

test =
  testProcess
    "Simple test"
    TestProcess
      { program = "test-executable-simple"
      , arguments = []
      , workingDir = Nothing
      , environment = Nothing
      , input = Nothing
      , exitCodeCheck = ignoreExitCode
      , stdoutCheck = \s -> if s == "Hello, World!\n" then Right () else Left "unexpected output"
      , stderrCheck = ignoreOutput
      , timeout = 1000000
      }

test =
  testProcess
    "Echo test"
    TestProcess
      { program = "test-executable-echo"
      , arguments = []
      , workingDir = Nothing
      , environment = Nothing
      , input = Just "Echo!"
      , exitCodeCheck = ignoreExitCode
      , stdoutCheck = \s -> if s == "Echo!" then Right () else Left "unexpected output"
      , stderrCheck = ignoreOutput
      , timeout = 1000000
      }
