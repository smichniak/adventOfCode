module Day18 where

import Data.List.Split (splitOn)
import qualified Data.Map as Map (Map, elems, empty, filter, insert, keys, member, (!))
import Data.NumInstances ()
import qualified Data.Set as Set (Set, fromList, map, member, toList)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Utils (DayInput, DayMain, DaySolution, readInt, standardMain, take3)

type Coordinate = (Int, Int, Int)

type AirSection = Map.Map Coordinate Int

type InputType = Set.Set Coordinate

dx3 :: [Int]
dx3 = [-1, 1, 0, 0, 0, 0]

dy3 :: [Int]
dy3 = [0, 0, -1, 1, 0, 0]

dz3 :: [Int]
dz3 = [0, 0, 0, 0, -1, 1]

inputParser :: DayInput InputType
inputParser = Set.fromList . map (take3 . map readInt . splitOn ",") . lines

getAdjacent :: Coordinate -> [Coordinate]
getAdjacent point = zipWith3 (\i j k -> point + (i, j, k)) dx3 dy3 dz3

countNeighbours :: Set.Set Coordinate -> Coordinate -> Int
countNeighbours points location =
  let isNeighbour = map (`Set.member` points) (getAdjacent location)
   in length $ filter id isNeighbour

solution1 :: DaySolution InputType
solution1 points = 6 * length points - sum (map (countNeighbours points) (Set.toList points))

tripleFun :: (a -> b) -> (a, a, a) -> (b, b, b)
tripleFun f (x, y, z) = (f x, f y, f z)

minPoint :: InputType -> Coordinate
minPoint points = tripleFun minimum (Set.map fst3 points, Set.map snd3 points, Set.map thd3 points)

maxPoint :: InputType -> Coordinate
maxPoint points = tripleFun maximum (Set.map fst3 points, Set.map snd3 points, Set.map thd3 points)

comparePoints :: Coordinate -> Coordinate -> Bool
comparePoints (x, y, z) (i, j, k) = x < i && y < j && z < k

correctAir :: Coordinate -> Coordinate -> Set.Set Coordinate -> Coordinate -> Bool
correctAir minP maxP lavaPoints location =
  comparePoints location maxP && comparePoints minP location && not (Set.member location lavaPoints)

getAirAdjacent :: Coordinate -> Coordinate -> Set.Set Coordinate -> Coordinate -> [Coordinate]
getAirAdjacent minP maxP lavaPoints location = filter (correctAir minP maxP lavaPoints) (getAdjacent location)

airSections :: Coordinate -> Coordinate -> Set.Set Coordinate -> [Coordinate] -> Int -> AirSection -> AirSection
airSections minP maxP lavaPoints [] _ sections = sections
airSections minP maxP lavaPoints (point : rest) currentSection sections =
  let airAdjacent = getAirAdjacent minP maxP lavaPoints point
      newRest = airAdjacent ++ rest
   in if Map.member point sections
        then airSections minP maxP lavaPoints rest currentSection sections
        else airSections minP maxP lavaPoints newRest currentSection (Map.insert point currentSection sections)

offset :: Coordinate
offset = (3, 3, 3)

solution2 :: DaySolution InputType
solution2 lavaPoints =
  let minP = minPoint lavaPoints - offset
      maxP = maxPoint lavaPoints + offset
      airPoint = (minP + (1, 1, 1))
      airPoints = airPoint : filter (correctAir minP maxP lavaPoints) (concatMap getAdjacent lavaPoints)
      sections = foldr (\(point, secNum) acc -> airSections minP maxP lavaPoints [point] secNum acc) Map.empty (zip airPoints [1 ..])
      outsideSection = sections Map.! airPoint
      insideAir = Set.fromList $ Map.keys $ Map.filter (outsideSection /=) sections
      outerSurface = solution1 lavaPoints - sum (map (countNeighbours insideAir) (Set.toList lavaPoints))
   in outerSurface

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
