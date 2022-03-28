module Main where

import Lib (makeWordMapFromStdin)

main :: IO ()
main = makeWordMapFromStdin >>= print
