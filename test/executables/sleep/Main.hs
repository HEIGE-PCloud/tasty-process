module Main (main) where

import Control.Concurrent (threadDelay)

-- | sleep for 10 seconds
main :: IO ()
main = threadDelay $ 10 * 1000000
