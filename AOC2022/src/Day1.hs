module Day1 where

import Data.List (sort)
import Data.List.Split (splitOn)
import Utils (readInt)

inputReader :: String -> [[Int]]
inputReader s = map (map readInt . lines) (splitOn "\n\n" s)

solution1 :: [[Int]] -> Int
solution1 = foldr (max . sum) 0

solution2 :: [[Int]] -> Int
solution2 elfs = sum $ drop (length elfs - 3) (sort $ map sum elfs)

main1 :: String -> Int
main1 = solution1 . inputReader

main2 :: String -> Int
main2 = solution2 . inputReader
