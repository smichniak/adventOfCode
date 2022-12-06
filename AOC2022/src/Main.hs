{-# LANGUAGE ViewPatterns #-}

module Main where

import Solutions (solutions)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Utils (inputFile)

lastDay :: Int
lastDay = length solutions `div` 2

printDaySolution :: Int -> Int -> IO ()
printDaySolution day part = do
  fileContent <- readFile $ inputFile day
  result <- (solutions !! ((day - 1) * 2 + part - 1)) fileContent
  putStrLn result

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "Advent of Code 2022 - Haskell solutions",
        "Usage: Call with one of the following argument combinations:",
        "  --help         Display this help message.",
        "  last           Print solution for both parts of the last day.",
        "  all            Print solution for all days and parts.",
        "  (day)          Print solution for both parts of a given day.",
        "  (day) (part)   Print solution for a given day and part."
      ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No arguments provided"
    ["--help"] -> usage
    ["last"] -> printDaySolution lastDay 1 >> printDaySolution lastDay 2
    ["all"] -> mapM_ (\x -> putStrLn ("Day " ++ show x ++ ":") >> printDaySolution x 1 >> printDaySolution x 2) [1 .. lastDay]
    [readMaybe -> Just day, readMaybe -> Just part] -> printDaySolution day part
    [readMaybe -> Just day] -> printDaySolution day 1 >> printDaySolution day 2
    _ -> putStrLn "Wrong arguments provided"
