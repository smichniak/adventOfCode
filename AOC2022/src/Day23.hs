module Day23 where

import Control.Applicative ((<|>))
import Data.List (elemIndices, findIndex, singleton)
import qualified Data.Map as Map (Map, filter, fromListWith, map, toList)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set (Set, delete, empty, fromDistinctAscList, insert, member, size, toList, union)
import Data.Tuple (swap)
import Utils (DayInput, DayMain, DaySolution, rotate, standardMain)

type Coordinate = (Int, Int)

type InputType = Set.Set Coordinate

data Direction = North | South | West | East

addRowIndices :: Int -> String -> InputType -> InputType
addRowIndices row = Set.union . Set.fromDistinctAscList . zip (repeat row) . elemIndices '#'

inputParser :: DayInput InputType
inputParser = foldr (uncurry addRowIndices) Set.empty . zip [0 ..] . lines

minRectangle :: InputType -> Int
minRectangle elves =
  let ys = map fst $ Set.toList elves
      xs = map snd $ Set.toList elves
   in (maximum ys - minimum ys + 1) * (maximum xs - minimum xs + 1)

hasNeighbours :: InputType -> Coordinate -> Bool
hasNeighbours elves (row, col) =
  let adjacent = filter (/= (row, col)) [(row + i, col + j) | i <- [-1 .. 1], j <- [-1 .. 1]]
   in any (`Set.member` elves) adjacent

getProposition :: InputType -> Coordinate -> Direction -> Maybe Coordinate
getProposition elves (row, col) North = if any (`Set.member` elves) [(row - 1, col + i) | i <- [-1 .. 1]] then Nothing else Just (row - 1, col)
getProposition elves (row, col) South = if any (`Set.member` elves) [(row + 1, col + i) | i <- [-1 .. 1]] then Nothing else Just (row + 1, col)
getProposition elves (row, col) West = if any (`Set.member` elves) [(row + i, col - 1) | i <- [-1 .. 1]] then Nothing else Just (row, col - 1)
getProposition elves (row, col) East = if any (`Set.member` elves) [(row + i, col + 1) | i <- [-1 .. 1]] then Nothing else Just (row, col + 1)

getElfProposition :: InputType -> [Direction] -> Coordinate -> Maybe Coordinate
getElfProposition elves directions position =
  if not $ hasNeighbours elves position
    then Nothing
    else foldr ((<|>) . getProposition elves position) Nothing directions

maybePair :: [(a, Maybe b)] -> [Maybe (a, b)]
maybePair [] = []
maybePair ((a, Nothing) : rest) = Nothing : maybePair rest
maybePair ((a, Just b) : rest) = Just (a, b) : maybePair rest

proposePositions :: InputType -> [Direction] -> Map.Map Coordinate [Coordinate]
proposePositions elves directionOrder =
  let elfList = Set.toList elves
      maybePropositions = zip (map singleton elfList) (map (getElfProposition elves directionOrder) elfList)
      propositions = map swap $ catMaybes $ maybePair maybePropositions
   in Map.fromListWith (++) propositions

directions :: [Direction]
directions = [North, South, West, East]

doRound :: InputType -> Int -> InputType
doRound elves roundNum =
  let directionOrder = rotate roundNum directions
      propositions = proposePositions elves directionOrder
      uniquePropositions = Map.map head $ Map.filter ((== 1) . length) propositions
      swappedPropositions = Map.toList uniquePropositions
   in foldr (\(target, src) -> Set.insert target . Set.delete src) elves swappedPropositions

numRounds :: Int
numRounds = 10

solution1 :: DaySolution InputType
solution1 elves =
  let afterRounds = foldl doRound elves [0 .. numRounds - 1]
   in minRectangle afterRounds - Set.size afterRounds

solution2 :: DaySolution InputType
solution2 elves =
  let afterRounds = scanl doRound elves [0 ..]
   in 1 + fromJust (findIndex (uncurry (==)) (zip afterRounds $ tail afterRounds))

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
