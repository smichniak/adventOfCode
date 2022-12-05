module Day2 where

import Data.Char (ord)
import Utils (DayInput, DayMain, DaySolution, standardMain)

type InputType = [(Int, Int)]

inputParser :: DayInput InputType
inputParser = map (\x -> (ord (head x) - ord 'A', ord (last x) - ord 'X')) . lines

points1 :: Int -> Int -> Int
points1 opponent me = me + 1 + 3 * ((me - opponent + 1) `mod` 3)

points2 :: Int -> Int -> Int
points2 opponent me = 1 + 3 * me + (opponent + me - 1) `mod` 3

solution1 :: DaySolution InputType
solution1 = sum . map (uncurry points1)

solution2 :: DaySolution InputType
solution2 = sum . map (uncurry points2)

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
