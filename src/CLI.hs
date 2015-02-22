{-|
Module      : CLI
Description : Functions dealing with the command line
Copyright   : (c) Carter Hinsley, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module CLI where

import Conversions

-- | Parse command line arguments and perform an action (hopefully translation)
-- based on the contents.
parseArgs :: [String] -> IO ()
parseArgs ["-g"] = putStr . asciiTextToG =<< getContents
parseArgs ["-a"] = putStr . gTextToAscii =<< getContents
parseArgs []     = putStrLn "No argument provided. Try `-g' or `-a'."
parseArgs _      = putStrLn "Invalid arguments."
