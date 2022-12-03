module Day3 where

import Data.Char (ord)
import Data.List.Split (chunksOf)
import Utils (DayInput, DayMain, DaySolution, standardMain)

type InputType1 = [(String, String)]

type InputType2 = [[String]]

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt (length l `div` 2) l

inputReader1 :: DayInput InputType1
inputReader1 s = map splitHalf (lines s)

inputReader2 :: DayInput InputType2
inputReader2 s = chunksOf 3 (lines s)

letterScore :: Char -> Int
letterScore c = if c <= 'Z' then ord c - ord 'A' + 27 else ord c - ord 'a' + 1

intersection :: String -> String -> [Char]
intersection [] _ = []
intersection (c : rest) s2 = if c `elem` s2 then c : intersection rest s2 else intersection rest s2

solution1 :: DaySolution InputType1
solution1 = sum . map (letterScore . head . uncurry intersection)

solution2 :: DaySolution InputType2
solution2 = sum . map (letterScore . head . foldr intersection (['a' .. 'z'] ++ ['A' .. 'Z']))

main1 :: DayMain
main1 = standardMain solution1 inputReader1

main2 :: DayMain
main2 = standardMain solution2 inputReader2
