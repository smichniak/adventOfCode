module Day22 where

import Data.List (elemIndex, elemIndices, findIndices, transpose)
import qualified Data.Map as Map (Map, fromAscList, lookup, (!))
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set (Set, fromList, lookupGT, lookupLT, member)
import Text.Parsec (anyChar, many, (<|>))
import Text.Parsec.String (Parser)
import Utils (DayInput, DayMain, DaySolution, getNext, intParser, parseString, standardMain)

type LineRocks = Set.Set Int

data MapLine = MapLine {start :: Int, len :: Int, rocks :: LineRocks} deriving (Show)

type LineMap = Map.Map Int MapLine

data MonkeyMap = MMap {rows :: LineMap, columns :: LineMap} deriving (Show)

data Rotation = Clockwise | Counterclockwise deriving (Show)

data Orientation = LeftO | RightO | UpO | DownO deriving (Show, Eq)

data Move = Mov Int | Rotate Rotation deriving (Show)

data Position = Pos {row :: Int, column :: Int, orientation :: Orientation} deriving (Show)

type InputType = (MonkeyMap, [Move])

getRot :: Char -> Rotation
getRot 'R' = Clockwise
getRot 'L' = Counterclockwise

parseMoves :: Parser [Move]
parseMoves = many (Mov <$> intParser <|> Rotate . getRot <$> anyChar)

parseMapRow :: String -> MapLine
parseMapRow s =
  let mapIndices = findIndices (/= ' ') s
      start = 1 + head mapIndices
      end = 1 + last mapIndices
      len = end - start + 1
      rocks = Set.fromList $ [id, (+ len), (+ (-len))] <*> map (+ 1) (elemIndices '#' s)
   in MapLine start len rocks

getLineMap :: [String] -> LineMap
getLineMap mapLines = Map.fromAscList $ zip [1 ..] (map parseMapRow mapLines)

getMonkeyMap :: [String] -> MonkeyMap
getMonkeyMap inputLines = MMap (getLineMap inputLines) (getLineMap $ transpose inputLines)

inputParser :: DayInput InputType
inputParser s =
  let inputLines = lines s
      mapLines = take (length inputLines - 2) inputLines
      moveLine = last inputLines
   in (getMonkeyMap mapLines, parseString parseMoves moveLine)

rotClockwise :: [Orientation]
rotClockwise = [RightO, DownO, LeftO, UpO]

moveInLine :: MapLine -> Int -> Int -> Int
moveInLine (MapLine start len rocks) position distance =
  let distanceToMove = if distance > 0 then distance `mod` len else (distance `mod` len) - len
      nextRock = if distance > 0 then Set.lookupGT position rocks else Set.lookupLT position rocks
      targetPosition = position + distance
      passedRock = (isJust nextRock && (abs (targetPosition - position) >= abs (fromJust nextRock - position)))
      newPosition = if passedRock then fromJust nextRock - signum distance else targetPosition
   in ((newPosition - start) `mod` len) + start

makeMove :: MonkeyMap -> Position -> Move -> Position
makeMove _ (Pos r c o) (Rotate Clockwise) = Pos r c (fromJust $ getNext o rotClockwise)
makeMove _ (Pos r c o) (Rotate Counterclockwise) = Pos r c (fromJust $ getNext o $ reverse rotClockwise)
makeMove monkeyMap (Pos row col RightO) (Mov distance) = Pos row (moveInLine (rows monkeyMap Map.! row) col distance) RightO
makeMove monkeyMap (Pos row col LeftO) (Mov distance) = Pos row (moveInLine (rows monkeyMap Map.! row) col (-distance)) LeftO
makeMove monkeyMap (Pos row col DownO) (Mov distance) = Pos (moveInLine (columns monkeyMap Map.! col) row distance) col DownO
makeMove monkeyMap (Pos row col UpO) (Mov distance) = Pos (moveInLine (columns monkeyMap Map.! col) row (-distance)) col UpO

move :: MonkeyMap -> Position -> [Move] -> Position
move = foldl . makeMove

orientationVal :: Orientation -> Int
orientationVal o = fromJust $ elemIndex o rotClockwise

getPassword :: Position -> Int
getPassword (Pos row col orientation) = 1000 * row + 4 * col + orientationVal orientation

solution1 :: DaySolution InputType
solution1 (monkeyMap, moves) =
  let ininitalPosition = Pos 1 (start $ rows monkeyMap Map.! 1) RightO
   in getPassword $ move monkeyMap ininitalPosition moves

isInBounds :: MonkeyMap -> Position -> Bool
isInBounds (MMap rows columns) (Pos row col _) =
  let rowMap = Map.lookup row rows
      colMap = Map.lookup col columns
      MapLine rowStart rowLen _ = fromJust rowMap
      MapLine colStart colLen _ = fromJust colMap
   in isJust rowMap
        && isJust colMap
        && rowStart <= col
        && col <= rowStart + rowLen - 1
        && colStart <= row
        && row <= colStart + colLen - 1

stepOnePosition :: Position -> Position
stepOnePosition (Pos row col RightO) = Pos row (col + 1) RightO
stepOnePosition (Pos row col LeftO) = Pos row (col - 1) LeftO
stepOnePosition (Pos row col DownO) = Pos (row + 1) col DownO
stepOnePosition (Pos row col UpO) = Pos (row - 1) col UpO

cubeSize :: Int
cubeSize = 50

wrapOnCube :: MonkeyMap -> Position -> Position
wrapOnCube (MMap rows columns) (Pos row _ RightO)
  | row <= cubeSize = Pos (3 * cubeSize - row + 1) (2 * cubeSize) LeftO
  | row <= 2 * cubeSize = Pos cubeSize (row + cubeSize) UpO
  | row <= 3 * cubeSize = Pos (3 * cubeSize - row + 1) (3 * cubeSize) LeftO
  | otherwise = Pos (3 * cubeSize) (row - 2 * cubeSize) UpO
wrapOnCube (MMap rows columns) (Pos row _ LeftO)
  | row <= cubeSize = Pos (3 * cubeSize - row + 1) 1 RightO
  | row <= 2 * cubeSize = Pos (2 * cubeSize + 1) (row - cubeSize) DownO
  | row <= 3 * cubeSize = Pos (3 * cubeSize - row + 1) (cubeSize + 1) RightO
  | otherwise = Pos 1 (row - 2 * cubeSize) DownO
wrapOnCube (MMap rows columns) (Pos _ col DownO)
  | col <= cubeSize = Pos 1 (col + 2 * cubeSize) DownO
  | col <= 2 * cubeSize = Pos (col + 2 * cubeSize) cubeSize LeftO
  | otherwise = Pos (col - cubeSize) (2 * cubeSize) LeftO
wrapOnCube (MMap rows columns) (Pos _ col UpO)
  | col <= cubeSize = Pos (col + cubeSize) (cubeSize + 1) RightO
  | col <= 2 * cubeSize = Pos (col + 2 * cubeSize) 1 RightO
  | otherwise = Pos (4 * cubeSize) (col - 2 * cubeSize) UpO

getNextOnCube :: MonkeyMap -> Position -> Position
getNextOnCube monkeyMap pos =
  let afterStep = stepOnePosition pos
   in if isInBounds monkeyMap afterStep then afterStep else wrapOnCube monkeyMap afterStep

moveByOne :: MonkeyMap -> Position -> Position
moveByOne monkeyMap pos =
  let newPos@(Pos newRow newCol _) = getNextOnCube monkeyMap pos
   in if Set.member newCol (rocks $ rows monkeyMap Map.! newRow) then pos else newPos

makeMoveOnCube :: MonkeyMap -> Position -> Move -> Position
makeMoveOnCube monkeyMap pos (Rotate o) = makeMove monkeyMap pos (Rotate o)
makeMoveOnCube monkeyMap pos (Mov 0) = pos
makeMoveOnCube monkeyMap pos (Mov distance) = makeMoveOnCube monkeyMap (moveByOne monkeyMap pos) (Mov $ distance - 1)

moveOnCube :: MonkeyMap -> Position -> [Move] -> Position
moveOnCube = foldl . makeMoveOnCube

solution2 :: DaySolution InputType
solution2 (monkeyMap, moves) =
  let ininitalPosition = Pos 1 (start $ rows monkeyMap Map.! 1) RightO
   in getPassword $ moveOnCube monkeyMap ininitalPosition moves

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
