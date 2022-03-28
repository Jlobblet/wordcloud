module Main where

import Lib

main :: IO ()
main = makeWordMap >>= print
