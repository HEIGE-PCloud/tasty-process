# tasty-process

`tasty-process` is a library for running integration tests with the [Tasty](https://github.com/UnkindPartition/tasty) testing framework.

Features:

- Run an external process as a test case
- Supply custom input to `stdin`
- Check `exitcode`, `stdout` and `stderr`
- Set timeout using Tasty's `Timeout` option
- Automatic clean up after the test

## Example

Here is an example Echo program that reads from `stdin` and echos back to `stdout`.

```hs
module Main (main) where

main :: IO ()
main = getLine >>= putStr
```

And here is a test case for the Echo program using `tasty-process`.

```hs
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree)
import Test.Tasty.Process

echoTest :: TestTree
echoTest =
  setTimeout (1000000) $ -- set timeout to 1 second
    processTest
      "Echo test" -- test name
      TestProcess
        { process =
          (proc "echo-test" []) -- process to launch with a list of arguments
        , input = "Echo!" -- input to stdin
        , exitCodeCheck = equals ExitSuccess -- check exit code
        , stdoutCheck = equals "Echo!" -- check stdout
        , stderrCheck = equals "" -- check stderr
        }

```

## Documentation

See the [Hackage page](https://hackage.haskell.org/package/tasty-process) for detailed API documentation.

## GHC Compatibility

`tasty-process` is tested with GHC version >= 8.6
