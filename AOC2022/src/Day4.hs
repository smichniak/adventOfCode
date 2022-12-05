module Day4 where

import Data.List.Split (splitOneOf)
import Utils (DayInput, DayMain, DaySolution, fromBool, readInt, standardMain)

type InputType = [[Int]]

inputParser :: DayInput InputType
inputParser s = map (map readInt . splitOneOf "-,") (lines s)

solution1 :: DaySolution InputType
solution1 = sum . map (\[x, y, z, w] -> fromBool (z >= x && w <= y || x >= z && y <= w))

solution2 :: DaySolution InputType
solution2 = sum . map (\[x, y, z, w] -> fromBool (x <= z && z <= y || x <= w && w <= y || z <= x && x <= w || z <= y && y <= w))

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
