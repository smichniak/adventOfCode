module Day12 where

import Data.Array (Array, assocs, bounds, listArray, (!), (//))
import Data.Bifunctor (bimap)
import Data.Char (ord)
import Data.Ix (Ix, inRange)
import Data.List (find, singleton)
import Data.Maybe (fromJust)
import Data.Sequence (Seq (Empty, (:<|)), fromList, (><))
import qualified Data.Sequence as Seq (singleton)
import qualified Data.Set as Set (empty, insert, member)
import Utils (DayInput, DayMain, DaySolution, dx, dy, standardMain)

type Coordinates = (Int, Int)

type Grid = Array Coordinates Int

type InputType = Grid

type CanStep = Grid -> Coordinates -> Coordinates -> Bool

minLevel :: Char
minLevel = 'a'

maxLevel :: Char
maxLevel = 'z'

startChar :: Char
startChar = 'S'

endChar :: Char
endChar = 'E'

canStep :: CanStep
canStep grid src dst = (grid ! dst) <= (grid ! src) + 1

canStepDown :: CanStep
canStepDown grid = flip $ canStep grid

getNeighbours :: CanStep -> Grid -> Coordinates -> [Coordinates]
getNeighbours stepFun arr src =
  let neighbours = zipWith (\y x -> bimap (+ y) (+ x) src) dy dx
      validNeighbourPredicates = [inRange $ bounds arr, stepFun arr src]
   in filter (and . (<*>) validNeighbourPredicates . singleton) neighbours

bfs :: (Ord a) => a -> (a -> [a]) -> [(Int, a)]
bfs start neighbors = bfs' (Seq.singleton (0, start)) Set.empty
  where
    bfs' Empty _ = []
    bfs' ((depth, x) :<| xs) visited
      | Set.member x visited = bfs' xs visited
      | otherwise = (depth, x) : bfs' (xs >< fromList (zip (repeat $ depth + 1) (neighbors x))) (Set.insert x visited)

listTo2DArray :: [[Int]] -> Grid
listTo2DArray xss =
  let rows = length xss
      cols = length (head xss)
   in listArray ((0, 0), (rows - 1, cols - 1)) $ concat xss

findIndex :: (Eq a, Ix i) => a -> Array i a -> i
findIndex x = fst . head . filter ((x ==) . snd) . assocs

clearGrid :: Grid -> (Coordinates, Coordinates, Grid)
clearGrid grid =
  let start = findIndex (ord startChar) grid
      end = findIndex (ord endChar) grid
      newGrid = grid // [(start, ord minLevel), (end, ord maxLevel)]
   in (start, end, newGrid)

inputParser :: DayInput InputType
inputParser = listTo2DArray . map (map ord) . lines

solution1 :: DaySolution InputType
solution1 grid =
  let (start, end, newGrid) = clearGrid grid
      bfsOrder = bfs start (getNeighbours canStep newGrid)
   in fst $ fromJust $ find ((== end) . snd) bfsOrder

solution2 :: DaySolution InputType
solution2 grid =
  let (_, end, newGrid) = clearGrid grid
      bfsOrder = bfs end (getNeighbours canStepDown newGrid)
   in minimum $ map fst $ filter ((ord minLevel ==) . (!) newGrid . snd) bfsOrder

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
