module Day8 where

import Data.Bifunctor (second)
import Data.List (singleton, transpose)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Utils (DayInput, DayMain, DaySolution, condInsert, partitions, readInt, standardMain)

type Coordinates = Set.Set (Int, Int)

type Row = [Int]

type InputType = [Row]

type CoordinatesTransform = Coordinates -> Coordinates

type GridTransform = InputType -> InputType

type RowTransform = Row -> Row

inputParser :: DayInput InputType
inputParser s = map (map (readInt . singleton)) (lines s)

type Accum = (Int, Coordinates)

getNewAccum :: Int -> Accum -> (Int, Int) -> Accum
getNewAccum row (currMax, visible) (column, x) = (max x currMax, condInsert (x > currMax) (row, column) visible)

getVisiblePoints :: Int -> Row -> Coordinates
getVisiblePoints rowNum = snd . foldl (getNewAccum rowNum) (-1, Set.empty) . zip [0 ..]

transposeSet :: Coordinates -> Coordinates
transposeSet = Set.map swap

reverseRow :: Int -> Coordinates -> Coordinates
reverseRow n = Set.map $ second ((n - 1) -)

getRowVisible :: CoordinatesTransform -> RowTransform -> GridTransform -> InputType -> [Coordinates]
getRowVisible coordinateTrans rowTrans gridTrans =
  zipWith (\rowNum -> coordinateTrans . getVisiblePoints rowNum . rowTrans) [0 ..] . gridTrans

solution1 :: DaySolution InputType
solution1 grid =
  let n = length $ head grid
      fromLeft = getRowVisible id id id grid
      fromRight = getRowVisible (reverseRow n) reverse id grid
      fromTop = getRowVisible transposeSet id transpose grid
      fromBottom = getRowVisible (transposeSet . reverseRow n) reverse transpose grid
   in length $ Set.unions $ concat [fromLeft, fromRight, fromTop, fromBottom]

oneSideVisibility :: Int -> Row -> Int
oneSideVisibility _ [] = 0
oneSideVisibility h (x : rest) = if x >= h then 1 else 1 + oneSideVisibility h rest

pointVisibility :: Row -> Row -> Int
pointVisibility left right =
  let currentHeight = head right
      leftVis = oneSideVisibility currentHeight (reverse left)
      rightVis = oneSideVisibility currentHeight (tail right)
   in leftVis * rightVis

getRowVisiblity :: Row -> [Int]
getRowVisiblity = map (uncurry pointVisibility) . reverse . tail . partitions

solution2 :: DaySolution InputType
solution2 grid =
  let rowVisibility = map getRowVisiblity grid
      columnVisibility = transpose $ map getRowVisiblity $ transpose grid
      visibility = zipWith (zipWith (*)) rowVisibility columnVisibility
   in maximum (map maximum visibility)

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
