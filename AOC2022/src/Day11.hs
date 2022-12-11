module Day11 where

import Data.Foldable (toList)
import Data.List (sort)
import Data.List.Split (chunksOf, splitOn)
import Data.Sequence (Seq, adjust, fromList, index)
import qualified Data.Sequence as Seq (length)
import Utils (DayInput, DayMain, DaySolution, butLast, compose, readInt, standardMain)

data Monkey = Monkey {items :: [Int], operation :: Int -> Int, throw :: Int -> Int, inspections :: Int}

type InputType = Seq Monkey

type WorryTransform = Int -> Int

monkeyRows :: Int
monkeyRows = 7

rounds1 :: Int
rounds1 = 20

rounds2 :: Int
rounds2 = 10000

getStartingItems :: String -> [Int]
getStartingItems = map readInt . splitOn "," . last . splitOn ":"

getOp :: Char -> (Int -> Int -> Int)
getOp '+' = (+)
getOp '*' = (*)

getOperation :: String -> (Int -> Int)
getOperation s =
  let parts = splitOn " " s
      numStr = last parts
      op = getOp $ head $ butLast parts
   in if numStr == "old"
        then \x -> op x x
        else op (readInt numStr)

getThrow :: [String] -> (Int -> Int)
getThrow s =
  let nums = map (readInt . last . splitOn " ") $ init s
   in \x -> if x `mod` head nums == 0 then nums !! 1 else nums !! 2

pareseMonkey :: [String] -> Monkey
pareseMonkey ls = Monkey (getStartingItems $ ls !! 1) (getOperation $ ls !! 2) (getThrow $ drop 3 ls) 0

inputParser :: DayInput InputType
inputParser = fromList . map pareseMonkey . chunksOf monkeyRows . lines

clearItems :: Monkey -> Monkey
clearItems (Monkey itemList op th insp) = Monkey [] op th (insp + length itemList)

addItem :: Int -> Monkey -> Monkey
addItem x (Monkey itemList op th insp) = Monkey (x : itemList) op th insp

throwItem :: WorryTransform -> Monkey -> InputType -> Int -> InputType
throwItem f currentMonkey monkeys x =
  let worryLevel = f $ operation currentMonkey x
      newMonkey = throw currentMonkey worryLevel
   in adjust (addItem worryLevel) newMonkey monkeys

turn :: WorryTransform -> InputType -> Int -> InputType
turn f monkeys monkeyIndex =
  let currentMonkey = index monkeys monkeyIndex
      newMonkeys = foldl (throwItem f currentMonkey) monkeys (items currentMonkey)
   in adjust clearItems monkeyIndex newMonkeys

throwRound :: WorryTransform -> InputType -> InputType
throwRound f monkeys = foldl (turn f) monkeys [0 .. Seq.length monkeys - 1]

performRounds :: WorryTransform -> Int -> InputType -> Int
performRounds worryTransform rounds monkeys =
  let afterRounds = compose rounds (throwRound worryTransform) monkeys
      inspectionList = sort $ map inspections $ toList afterRounds
   in last inspectionList * butLast inspectionList

solution1 :: DaySolution InputType
solution1 = performRounds (`div` 3) rounds1

getDivNum :: Monkey -> Int
getDivNum (Monkey _ _ throwFun _) =
  let targets = [(x, throwFun x) | x <- [1 ..]]
   in fst $ head $ filter ((/= (snd $ head targets)) . snd) targets

solution2 :: DaySolution InputType
solution2 monkeys =
  let moduloNum = product $ map getDivNum $ toList monkeys
   in performRounds (`mod` moduloNum) rounds2 monkeys

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
