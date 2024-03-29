module Day1 where

import Data.List (sort)
import Data.List.Split (splitOn)
import Utils (DayInput, DayMain, DaySolution, readInt, standardMain)

type InputType = [[Int]]

inputParser :: DayInput InputType
inputParser s = map (map readInt . lines) (splitOn "\n\n" s)

solution1 :: DaySolution InputType
solution1 = foldr (max . sum) 0

solution2 :: DaySolution InputType
solution2 elves = sum $ drop (length elves - 3) (sort $ map sum elves)

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
