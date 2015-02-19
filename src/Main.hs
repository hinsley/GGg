module Main where

import CLI
import System.Environment

main :: IO ()
main = parseArgs =<< getArgs

