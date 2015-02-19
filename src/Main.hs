module Main where

import Conversions
import CLI
import System.Environment

main :: IO ()
main = parseArgs =<< getArgs

