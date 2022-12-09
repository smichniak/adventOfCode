module Day9 where

import Data.Bifunctor (first, second)
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Utils (DayInput, DayMain, DaySolution, readInt, standardMain)

data MoveDirection = MUp | MDown | MLeft | MRight

data Move = Move MoveDirection Int

type InputType = [Move]

type Coordinate = (Int, Int)

type Coordinates = Set.Set Coordinate

makeMove :: Char -> Int -> Move
makeMove 'U' = Move MUp
makeMove 'D' = Move MDown
makeMove 'L' = Move MLeft
makeMove 'R' = Move MRight

inputParser :: DayInput InputType
inputParser = map ((\x -> makeMove (head $ head x) (readInt $ last x)) . splitOn " ") . lines

normalizeTail :: Coordinate -> Coordinate -> Coordinate
normalizeTail (hx, hy) (tx, ty)
  | abs (hx - tx) >= 2 || abs (hy - ty) >= 2 = (tx + signum (hx - tx), ty + signum (hy - ty))
  | otherwise = (tx + (hx - tx) `quot` 2, ty + (hy - ty) `quot` 2)

coordinateChange :: MoveDirection -> Coordinate -> Coordinate
coordinateChange MUp = second (1 +)
coordinateChange MDown = second ((-1) +)
coordinateChange MLeft = first ((-1) +)
coordinateChange MRight = first (1 +)

moveLongRope :: [Coordinate] -> Coordinates -> InputType -> Coordinates
moveLongRope _ visited [] = visited
moveLongRope positions visited (Move _ 0 : rest) = moveLongRope positions visited rest
moveLongRope (ropeHead : ropeTail) visited (Move dir x : rest) =
  let newHead = coordinateChange dir ropeHead
      newPositions = scanl normalizeTail newHead ropeTail
   in moveLongRope newPositions (Set.insert (last newPositions) visited) (Move dir (x - 1) : rest)

solution1 :: DaySolution InputType
solution1 = length . moveLongRope (replicate 2 (0, 0)) Set.empty

solution2 :: DaySolution InputType
solution2 = length . moveLongRope (replicate 10 (0, 0)) Set.empty

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
