module Day10 where

import Data.List.Split (chunksOf, splitOn)
import Utils (DayInput, DayMain, DaySolution, DayStringSolution, combine, readInt, standardMain, standardStringMain)

data Instruction = NOP | ADDX Int

type RegisterVal = Int

type CycleNum = Int

type InputType = [Instruction]

screenWidth :: Int
screenWidth = 40

interestingCycles :: [Int]
interestingCycles = [20, 60, 100, 140, 180, 220]

makeInstruction :: String -> Instruction
makeInstruction "noop" = NOP
makeInstruction s = ADDX $ readInt $ last $ splitOn " " s

inputParser :: DayInput InputType
inputParser = map makeInstruction . lines

serachIndices :: [Int] -> [a] -> [a]
serachIndices indices l = map snd $ filter ((`elem` indices) . fst) (zip [0 ..] l)

changeRegister :: [RegisterVal] -> Instruction -> [RegisterVal]
changeRegister regs NOP = head regs : regs
changeRegister regs (ADDX num) = (head regs + num) : head regs : regs

solution1 :: DaySolution InputType
solution1 = sum . zipWith (*) interestingCycles . serachIndices interestingCycles . reverse . foldl changeRegister [1, 0]

isClose :: CycleNum -> RegisterVal -> Bool
isClose cycle x = abs ((cycle `mod` screenWidth) - x) <= 1

drawPixel :: Bool -> Char
drawPixel True = '#'
drawPixel False = '.'

solution2 :: DayStringSolution InputType
solution2 instructions =
  let positions = reverse $ tail $ foldl changeRegister [1] instructions
      pixels = zipWith (combine drawPixel isClose) [0 ..] positions
   in unlines $ chunksOf screenWidth pixels

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardStringMain solution2 inputParser
