module Day24 where

import Data.Bifunctor (bimap)
import qualified Data.Map as Map (Map, empty, fromAscList, fromList, insertWith, keys, size, toList, union, (!))
import Data.Sequence (Seq ((:<|)), (><))
import qualified Data.Sequence as Seq (fromList, singleton)
import qualified Data.Set as Set (Set, empty, insert, member)
import Utils (DayInput, DayMain, DaySolution, dx, dy, standardMain)

type Coordinate = (Int, Int)

data Direction = North | South | West | East deriving (Show)

type InputType = Map.Map Coordinate [Direction]

type BlizzardStates = Map.Map Int InputType

getDirection :: Char -> [Direction]
getDirection '^' = [North]
getDirection 'v' = [South]
getDirection '<' = [West]
getDirection '>' = [East]
getDirection '.' = []

getRowBlizzard :: Int -> String -> InputType
getRowBlizzard row s =
  let coordinates = zip (repeat row) [0 ..]
      noEdges = filter (/= '#') s
   in Map.fromAscList (zip coordinates (map getDirection noEdges))

inputParser :: DayInput InputType
inputParser s =
  let sLines = lines s
      rows = length sLines
      noEdges = tail $ take (rows - 1) sLines
   in foldr (Map.union . uncurry getRowBlizzard) Map.empty $ zip [0 ..] noEdges

moveBlizzard :: Int -> Int -> Coordinate -> Direction -> InputType -> InputType
moveBlizzard rows _ (row, col) North = Map.insertWith (++) ((row - 1) `mod` rows, col) [North]
moveBlizzard rows _ (row, col) South = Map.insertWith (++) ((row + 1) `mod` rows, col) [South]
moveBlizzard _ cols (row, col) West = Map.insertWith (++) (row, (col - 1) `mod` cols) [West]
moveBlizzard _ cols (row, col) East = Map.insertWith (++) (row, (col + 1) `mod` cols) [East]

getRowsCols :: InputType -> Coordinate
getRowsCols blizzardState =
  let locations = Map.keys blizzardState
   in (1 + maximum (map fst locations), 1 + maximum (map snd locations))

getNextState :: InputType -> InputType
getNextState blizzardState =
  let locations = Map.keys blizzardState
      emptyNewState = Map.fromList (zip locations $ repeat [])
      (rows, cols) = getRowsCols blizzardState
      blizzardDirections = concatMap (uncurry (zip . repeat)) (Map.toList blizzardState)
   in foldr (uncurry (moveBlizzard rows cols)) emptyNewState blizzardDirections

getAllStates :: InputType -> BlizzardStates
getAllStates initialState =
  let locations = Map.keys initialState
      rows = 1 + maximum (map fst locations)
      cols = 1 + maximum (map snd locations)
      states = iterate getNextState initialState
   in Map.fromAscList $ zip [0 .. lcm rows cols - 1] states

getNeighbours :: Coordinate -> [Coordinate]
getNeighbours point = point : zipWith (\y x -> bimap (+ y) (+ x) point) dy dx

validLocation :: InputType -> Coordinate -> Coordinate -> Bool
validLocation blizzardState target point@(row, col) =
  let (rows, cols) = getRowsCols blizzardState
   in point == target || (0 <= row && row < rows && 0 <= col && col < cols && null (blizzardState Map.! point))

bfs :: Coordinate -> Seq (Int, Coordinate) -> Coordinate -> Set.Set (Int, Coordinate) -> BlizzardStates -> Int
bfs start ((depth, location) :<| rest) target visited allStates
  | Set.member (depth `mod` Map.size allStates, location) visited = bfs start rest target visited allStates
  | target `elem` validNeighbours = depth + 1
  | otherwise = bfs start (rest >< neighbours) target newVisited allStates
  where
    newState = allStates Map.! ((depth + 1) `mod` Map.size allStates)
    validNeighbours = filter (validLocation newState target) $ getNeighbours location
    neighboursWithStart = if location == start then location : validNeighbours else validNeighbours
    neighbours = Seq.fromList $ zip (repeat (depth + 1)) neighboursWithStart
    newVisited = Set.insert (depth `mod` Map.size allStates, location) visited

solution1 :: DaySolution InputType
solution1 blizzardState =
  let allStates = getAllStates blizzardState
      (rows, cols) = getRowsCols blizzardState
      start = (-1, 0)
      target = (rows, cols - 1)
   in bfs start (Seq.singleton (0, start)) target Set.empty allStates

solution2 :: DaySolution InputType
solution2 blizzardState =
  let allStates = getAllStates blizzardState
      (rows, cols) = getRowsCols blizzardState
      start = (-1, 0)
      target = (rows, cols - 1)
      firstTrip = bfs start (Seq.singleton (0, start)) target Set.empty allStates
      secondTrip = bfs target (Seq.singleton (firstTrip, target)) start Set.empty allStates
   in bfs start (Seq.singleton (secondTrip, start)) target Set.empty allStates

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
