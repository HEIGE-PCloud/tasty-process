module Main (main) where

import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  putStrLn "Hello, world!"
  -- print to stderr
  hPutStrLn stderr "Hello, stderr!"
