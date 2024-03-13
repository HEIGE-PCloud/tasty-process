module Main (main) where

import System.Exit (ExitCode (ExitFailure), exitWith)

main :: IO ()
main = exitWith (ExitFailure 123)
