{- AUTOCOLLECT.TEST -}

module Test (
{- AUTOCOLLECT.TEST.export -}

) where

import Test.Tasty.Process

test = testProcess "Simple test" TestProcess
  { program = "test-executable"
  , arguments = []
  , workingDir = Nothing
  , environment = Nothing
  , input = Nothing
  , exitCodeCheck = ignoreExitCode
  , stdoutCheck = \s -> if s == "Hello, World!\n" then Right () else Left "unexpected output"
  , stderrCheck = ignoreOutput
  , timeout = 1000000
  }

