module Main where

import Conversions
import System.Environment

main :: IO ()
main = parseArgs =<< getArgs

parseArgs :: [String] -> IO ()
parseArgs ["-g"]        = putStr . asciiTextToG =<< getContents
parseArgs ["-a"]        = putStr . gTextToAscii =<< getContents
parseArgs []            = putStrLn "No argument provided. Try `-g' or `-a'."
parseArgs args          = putStrLn "Invalid arguments."
