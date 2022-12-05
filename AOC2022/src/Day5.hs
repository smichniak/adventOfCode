module Day5 where

import Data.Array (Array, elems, listArray, (!), (//))
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Utils (DayInput, DayMain, DayStringSolution, standardStringMain)

type Arr a = Array Int a

type InputType = (Arr [Char], [[Int]])

getStack :: Int -> Int -> [String] -> [Char]
getStack _ 8 _ = []
getStack col row inLines =
  let letter = (inLines !! row) !! (4 * col + 1)
   in if letter /= ' '
        then letter : getStack col (row + 1) inLines
        else getStack col (row + 1) inLines

inputParser :: DayInput InputType
inputParser s =
  let inputLines = lines s
   in (listArray (0, 8) [getStack col 0 inputLines | col <- [0 .. 8]], map (mapMaybe readMaybe . splitOn " ") (drop 10 inputLines))

solution1 :: DayStringSolution InputType
solution1 (stacks, []) = map head $ elems stacks
solution1 (stacks, [0, source, target] : rest) = solution1 (stacks, rest)
solution1 (stacks, [num, source, target] : rest) =
  let letterToMove = head $ stacks ! (source - 1)
      newTarget = letterToMove : stacks ! (target - 1)
      newSource = tail $ stacks ! (source - 1)
   in solution1 (stacks // [(source - 1, newSource), (target - 1, newTarget)], [num - 1, source, target] : rest)

solution2 :: DayStringSolution InputType
solution2 (stacks, []) = map head $ elems stacks
solution2 (stacks, [num, source, target] : rest) =
  let lettersToMove = take num $ stacks ! (source - 1)
      newTarget = lettersToMove ++ stacks ! (target - 1)
      newSource = drop num $ stacks ! (source - 1)
   in solution2 (stacks // [(source - 1, newSource), (target - 1, newTarget)], rest)

main1 :: DayMain
main1 = standardStringMain solution1 inputParser

main2 :: DayMain
main2 = standardStringMain solution2 inputParser
