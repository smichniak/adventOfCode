module Day15 where

import Text.Parsec (between, many, many1, noneOf, sepBy)
import Text.Parsec.String (Parser)
import Utils (DayInput, DayMain, DaySolution, intParser, parseString, standardMain, take2)

type Coordinate = (Int, Int)

type SensorBeacon = (Coordinate, Coordinate)

type InputType = [SensorBeacon]

notNumber :: Parser String
notNumber = many1 $ noneOf ('-' : ['0' .. '9'])

lineParser :: Parser [Int]
lineParser = between (many notNumber) (many notNumber) (sepBy intParser (many notNumber))

parseLine :: String -> SensorBeacon
parseLine s =
  let nums = parseString lineParser s
   in (take2 nums, take2 $ drop 2 nums)

inputParser :: DayInput InputType
inputParser = map parseLine . lines

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

possibleBeacon :: Coordinate -> SensorBeacon -> Bool
possibleBeacon location (sensor, beacon) = location == beacon || manhattanDistance sensor location > manhattanDistance sensor beacon

yLevel :: Int
yLevel = 2000000

solution1 :: DaySolution InputType
solution1 beacons =
  let xs = concatMap (\((x1, _), (x2, _)) -> [x1, x2]) beacons
      minX = minimum xs
      maxX = maximum xs
   in length $ filter id $ map (\x -> (not . all (possibleBeacon (x, yLevel))) beacons) [2 * minX .. 2 * maxX]

connectingLine :: Coordinate -> Coordinate -> [Coordinate]
connectingLine (x1, y1) (x2, y2)
  | x1 == x2 = [(x1, y1)]
  | otherwise = (x1, y1) : connectingLine (x1 + signum (x2 - x1), y1 + signum (y2 - y1)) (x2, y2)

sensorRim :: SensorBeacon -> [Coordinate]
sensorRim (sensor@(xs, ys), beacon@(xb, yb)) =
  let d = manhattanDistance sensor beacon
      top = (xs, ys + d + 1)
      bottom = (xs, ys - d - 1)
      left = (xs - d - 1, ys)
      right = (xs + d + 1, ys)
   in concatMap (uncurry connectingLine) [(top, left), (top, right), (left, bottom), (right, bottom)]

minX2 :: Int
minX2 = 0

maxX2 :: Int
maxX2 = 4000000

minY2 :: Int
minY2 = 0

maxY2 :: Int
maxY2 = 4000000

xMultiplier :: Int
xMultiplier = 4000000

validCoordinate :: Coordinate -> Bool
validCoordinate (x, y) = minX2 <= x && x <= maxX2 && minY2 <= y && y <= maxY2

solution2 :: DaySolution InputType
solution2 beacons =
  let rims = filter validCoordinate $ concatMap sensorRim beacons
      (x, y) = head $ filter (flip all beacons . possibleBeacon) rims
   in xMultiplier * x + y

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
