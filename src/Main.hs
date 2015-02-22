{-|
Module      : Main
Description : Entry point for GGg
Copyright   : (c) Carter Hinsley, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module Main where

import CLI
import System.Environment

-- | Retrieve command-line arguments and process them.
main :: IO ()
main = parseArgs =<< getArgs

