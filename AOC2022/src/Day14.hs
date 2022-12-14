module Day14 where

import qualified Data.Map as Map
import Text.Parsec (char, digit, eof, many1, parse, sepBy, string)
import Text.Parsec.String (Parser)
import Utils (DayInput, DayMain, DaySolution, readInt, standardMain, take2)

type Coordinate = (Int, Int)

type InputType = [RockStructure]

type RockStructure = [Coordinate]

type Cave = Map.Map Coordinate Char

type OOBFunction = Coordinate -> Cave -> Bool

rock :: Char
rock = '#'

sand :: Char
sand = 'o'

sandGenerator :: Coordinate
sandGenerator = (500, 0)

intParser :: Parser Int
intParser = read <$> many1 digit

pairParser :: Parser Coordinate
pairParser = take2 <$> sepBy intParser (char ',')

structure :: Parser RockStructure
structure = pairParser `sepBy` string " -> "

parseLine :: String -> RockStructure
parseLine str =
  let Right result = parse (structure <* eof) "" str
   in result

inputParser :: DayInput InputType
inputParser = map parseLine . lines

drawLine :: Coordinate -> Coordinate -> Cave -> Cave
drawLine (x1, y1) (x2, y2) cave
  | x1 == x2 && y1 == y2 = Map.insert (x1, y1) '#' cave
  | y1 == y2 = drawLine (min x1 x2 + 1, y1) (max x1 x2, y2) (Map.insert (min x1 x2, y1) rock cave)
  | x1 == x2 = drawLine (x1, min y1 y2 + 1) (x2, max y1 y2) (Map.insert (x1, min y1 y2) rock cave)

drawStructures :: [RockStructure] -> Cave -> Cave
drawStructures [] = id
drawStructures ([_] : rest) = drawStructures rest
drawStructures ((form1 : form2 : restForms) : rest) = drawStructures ((form2 : restForms) : rest) . drawLine form1 form2

canMove :: Coordinate -> Cave -> Maybe Coordinate
canMove (i, j) cave
  | not $ Map.member (i, j + 1) cave = Just (i, j + 1)
  | not $ Map.member (i - 1, j + 1) cave = Just (i - 1, j + 1)
  | not $ Map.member (i + 1, j + 1) cave = Just (i + 1, j + 1)
  | otherwise = Nothing

moveSand :: OOBFunction -> Coordinate -> Cave -> Cave
moveSand oob position cave =
  if oob position cave
    then cave
    else case canMove position cave of
      Nothing -> moveSand oob sandGenerator (Map.insert position sand cave)
      Just newPosition -> moveSand oob newPosition cave

solution1 :: DaySolution InputType
solution1 formations =
  let cave = drawStructures formations Map.empty
      maxJ = maximum $ map snd $ Map.keys cave
      caveWithSand = moveSand (\(i, j) _ -> j > maxJ) sandGenerator cave
   in length $ filter (== sand) $ Map.elems caveWithSand

blockedGenerator :: OOBFunction
blockedGenerator _ = Map.member sandGenerator

solution2 :: DaySolution InputType
solution2 formations =
  let cave = drawStructures formations Map.empty
      maxJ = maximum $ map snd $ Map.keys cave
      minI = minimum $ map fst $ Map.keys cave
      maxI = maximum $ map fst $ Map.keys cave
      caveWithFloor = drawLine (minI - maxJ - 2, maxJ + 2) (maxI + maxJ + 2, maxJ + 2) cave
      caveWithSand = moveSand blockedGenerator sandGenerator caveWithFloor
   in length $ filter (== sand) $ Map.elems caveWithSand

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
