module CLI 
( parseArgs
) where

parseArgs :: [String] -> IO ()
parseArgs ["-g"] = putStr . asciiTextToG =<< getContents
parseArgs ["-a"] = putStr . gTextToAscii =<< getContents
parseArgs []     = putStrLn "No argument provided. Try `-g' or `-a'."
parseArgs _      = putStrLn "Invalid arguments."
