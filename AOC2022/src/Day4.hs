module Day4 where

import Data.List.Split (splitOneOf)
import Utils (DayInput, DayMain, DaySolution, readInt, standardMain)

type InputType = [[Int]]

inputParser :: DayInput InputType
inputParser s = map (map readInt . splitOneOf "-,") (lines s)

solution1 :: DaySolution InputType
solution1 = length . filter id . map (\[x, y, z, w] -> (x - z) * (w - y) >= 0)

solution2 :: DaySolution InputType
solution2 = length . filter id . map (\[x, y, z, w] -> (x - w) * (z - y) >= 0)

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
