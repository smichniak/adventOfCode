module Day17 where

import Data.Bifunctor (bimap)
import qualified Data.Set as Set (Set, empty, insert, member)
import Utils (DayInput, DayMain, DaySolution, standardMain)

data Move = MLeft | MRight

type InputType = [Move]

type Coordinate = (Int, Int)

type Rock = [Coordinate]

type Occupied = Set.Set Coordinate

type UnplacedRock = Int -> Int -> Rock

getMove :: Char -> Move
getMove '<' = MLeft
getMove '>' = MRight

inputParser :: DayInput InputType
inputParser = map getMove . concat . repeat . head . lines

rockNumber :: Int
rockNumber = 2022

caveWidth :: Int
caveWidth = 7

rockMinus :: Int -> Int -> Rock
rockMinus x y = [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)]

rockPlus :: Int -> Int -> Rock
rockPlus x y = [(x, y + 1), (x + 1, y), (x + 1, y + 1), (x + 1, y + 2), (x + 2, y + 1)]

rockL :: Int -> Int -> Rock
rockL x y = [(x, y), (x + 1, y), (x + 2, y), (x + 2, y + 1), (x + 2, y + 2)]

rockI :: Int -> Int -> Rock
rockI x y = [(x, y), (x, y + 1), (x, y + 2), (x, y + 3)]

rockSquare :: Int -> Int -> Rock
rockSquare x y = [(x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1)]

moveRock :: (Int -> Int) -> (Int -> Int) -> Rock -> Rock
moveRock xOp yOp = map (bimap xOp yOp)

moveDown :: Rock -> Rock
moveDown = moveRock id (+ (-1))

moveLeft :: Rock -> Rock
moveLeft = moveRock (+ (-1)) id

moveRight :: Rock -> Rock
moveRight = moveRock (+ 1) id

getRockMove :: Move -> (Rock -> Rock)
getRockMove MLeft = moveLeft
getRockMove MRight = moveRight

rocks :: [UnplacedRock]
rocks = cycle [rockMinus, rockPlus, rockL, rockI, rockSquare]

isOccupied :: Occupied -> Coordinate -> Bool
isOccupied occupied location@(x, y) = x <= 0 || x > caveWidth || y <= 0 || Set.member location occupied

placeRock :: Occupied -> Rock -> Occupied
placeRock = foldr Set.insert

getRockHigehest :: Rock -> Int
getRockHigehest = maximum . map snd

moveUntilStops :: Occupied -> Int -> Rock -> [Move] -> (Occupied, Int, [Move])
moveUntilStops occupied highest rock (move : restMoves) =
  let movedRock = getRockMove move rock
      newRock = if any (isOccupied occupied) movedRock then rock else movedRock
      downMoved = moveDown newRock
      shouldRest = any (isOccupied occupied) downMoved
   in if shouldRest
        then (placeRock occupied newRock, max highest $ getRockHigehest newRock, restMoves)
        else moveUntilStops occupied highest downMoved restMoves

dropRocks :: Occupied -> Int -> [Move] -> [UnplacedRock] -> Int
dropRocks _ highestPoint _ [] = highestPoint
dropRocks occupied highestPoint moves (rockToPlace : restRocks) =
  let rock = rockToPlace 3 (highestPoint + 4)
      (newOccupied, newHighest, newMoves) = moveUntilStops occupied highestPoint rock moves
   in dropRocks newOccupied newHighest newMoves restRocks

solution1 :: DaySolution InputType
solution1 m = dropRocks Set.empty 0 m (take rockNumber rocks)

rockNumber2 :: Int
rockNumber2 = 1000000000000

patternPeriod :: Int
patternPeriod = 1725 -- Found using Fourier Transform on sequence of first 4000000 heights

solution2 :: DaySolution InputType
solution2 m =
  let modRocks = rockNumber2 `mod` patternPeriod
      height1 = dropRocks Set.empty 0 m (take modRocks rocks)
      height2 = dropRocks Set.empty 0 m (take (modRocks + patternPeriod) rocks)
      patternLengthDiff = height2 - height1
   in height1 + patternLengthDiff * (rockNumber2 `div` patternPeriod)

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
