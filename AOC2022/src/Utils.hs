module Utils where

inputFile :: Int -> String
inputFile n = "input/day" ++ show n ++ ".in"

readInt :: String -> Int
readInt = read
