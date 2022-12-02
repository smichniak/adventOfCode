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
  print $ (solutions !! ((day - 1) * 2 + part - 1)) fileContent

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "Advent of Code 2022 - Haskell solutions",
        "Usage: Call with one of the following argument combinations:",
        "  --help         Display this help message.",
        "  last           First solution for both parts of the last day.",
        "  (day) (part)   Print solutions for a given day and part."
      ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No arguments provided"
    ["--help"] -> usage
    ["last"] -> printDaySolution lastDay 1 >> printDaySolution lastDay 2
    [readMaybe -> Just day, readMaybe -> Just part] -> printDaySolution day part
    _ -> putStrLn "Wrong arguments provided"
