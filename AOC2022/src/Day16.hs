module Day16 where

import Data.List (sort)
import qualified Data.Map as Map
import Data.Matrix (Matrix, getElem, mapPos, minorMatrix, multStd, setElem, toList, toLists, zero)
import qualified Data.Set as Set
import Text.Parsec (between, many, many1, noneOf, sepBy, (<|>))
import Text.Parsec.Char (digit, upper)
import Text.Parsec.String (Parser)
import Utils (DayInput, DayMain, DaySolution, parseString, readInt, standardMain)

data Valve = Val {flow :: Int, tunnels :: [String]}

type ValveMap = Map.Map String Valve

type ValveWorth = Map.Map (String, Int) Int

type InputType = ValveMap

notNumUpper :: Parser Char
notNumUpper = noneOf (['A' .. 'Z'] ++ ['0' .. '9'])

numOrUpper :: Parser Char
numOrUpper = digit <|> upper

divideLine :: Parser [String]
divideLine = between (upper *> many notNumUpper) (many notNumUpper) (sepBy (many1 numOrUpper) (many notNumUpper))

parseValve :: String -> ValveMap
parseValve s =
  let (valveName : flowRate : connections) = parseString divideLine s
   in Map.singleton valveName $ Val (readInt flowRate) connections

inputParser :: DayInput InputType
inputParser = foldr (Map.union . parseValve) Map.empty . lines

namesToNum :: [String] -> Map.Map String Int
namesToNum names = Map.fromList (zip (sort names) [1 ..])

numFlows :: InputType -> Map.Map Int Int
numFlows valves =
  let names = Map.keys valves
      nums = namesToNum names
   in Map.fromList (map (\name -> (nums Map.! name, flow $ valves Map.! name)) names)

addNeighbours :: Int -> Map.Map String Int -> [String] -> Matrix Int -> Matrix Int
addNeighbours source nameMap neighbours mat = foldr (\name -> setElem 1 (source, nameMap Map.! name)) mat neighbours

makeAdjacencyMatrix :: InputType -> Matrix Int
makeAdjacencyMatrix valves =
  let names = Map.keys valves
      nums = namesToNum names
      n = length names
      zeros = zero n n
   in foldr (\name -> addNeighbours (nums Map.! name) nums (tunnels $ valves Map.! name)) zeros names

isFilled :: Matrix Int -> Bool
isFilled = and . toList . mapPos (\(row, col) x -> x == 1 || row == col)

getShortestPaths :: Matrix Int -> Matrix Int
getShortestPaths adjacency =
  if isFilled adjacency
    then adjacency
    else
      let z = multStd adjacency adjacency
          b = mapPos (\(row, col) zElem -> if row /= col && (getElem row col adjacency == 1 || zElem > 0) then 1 else 0) z
          t = getShortestPaths b
          x = multStd t adjacency
          degree = map sum $ toLists adjacency
       in mapPos (\(row, col) tElem -> if getElem row col x >= tElem * degree !! (col - 1) then 2 * tElem else 2 * tElem - 1) t

nonZeroFlows :: InputType -> Matrix Int -> Matrix Int
nonZeroFlows valves distances =
  let numFlow = numFlows valves
      zeroFlowsNums = map fst $ filter (\(valveNum, flow) -> flow == 0 && valveNum /= 1) $ Map.toAscList numFlow
   in foldr (\x -> minorMatrix x x) distances zeroFlowsNums

getMaxPressure :: Int -> Int -> Map.Map Int Int -> Set.Set Int -> Matrix Int -> Int
getMaxPressure currentPosition timeLeft flows availableValves distances =
  if timeLeft <= 0
    then 0
    else
      let flowRate = flows Map.! currentPosition
          currentFlowValue = flowRate * (timeLeft - 1)
          removeSelf = Set.delete currentPosition availableValves
          otherValues = Set.map (\other -> getMaxPressure other (timeLeft - 1 - getElem currentPosition other distances) flows removeSelf distances) removeSelf
          valList = Set.toList otherValues
       in currentFlowValue + if null valList then 0 else maximum valList

startingValve :: String
startingValve = "AA"

timeLimit :: Int
timeLimit = 30

solution1 :: DaySolution InputType
solution1 valves =
  let adj = makeAdjacencyMatrix valves
      paths = getShortestPaths adj
      nonZeroPaths = nonZeroFlows valves paths

      nonZeroValves = Map.filterWithKey (\name (Val flow _) -> flow > 0 || name == startingValve) valves
      nonZeroNames = Map.keys nonZeroValves
      nonZeroNums = namesToNum nonZeroNames
      nonZeroFlowMap = numFlows nonZeroValves

      allValves = Set.fromList (Map.keys nonZeroFlowMap)
   in getMaxPressure (nonZeroNums Map.! startingValve) (timeLimit + 1) nonZeroFlowMap allValves nonZeroPaths

timeLimit2 :: Int
timeLimit2 = 26

solution2 :: DaySolution InputType
solution2 valves =
  let adj = makeAdjacencyMatrix valves
      paths = getShortestPaths adj
      nonZeroPaths = nonZeroFlows valves paths

      nonZeroValves = Map.filterWithKey (\name (Val flow _) -> flow > 0 || name == startingValve) valves
      nonZeroNames = Map.keys nonZeroValves
      nonZeroNums = namesToNum nonZeroNames
      nonZeroFlowMap = numFlows nonZeroValves

      allValves = Set.fromList (Map.keys nonZeroFlowMap)
      startingNum = nonZeroNums Map.! startingValve

      subsets = Set.powerSet (Set.delete startingNum allValves)
      mySets = sort $ Set.toList subsets
      myHalfSets = take (length mySets `div` 2) mySets

      elephantSets = map (Set.difference allValves) myHalfSets

      myWith1 = map (Set.insert startingNum) myHalfSets
      elephantWith1 = map (Set.insert startingNum) elephantSets
   in maximum $
        zipWith
          ( \my elephant ->
              getMaxPressure (nonZeroNums Map.! startingValve) (timeLimit2 + 1) nonZeroFlowMap my nonZeroPaths
                + getMaxPressure (nonZeroNums Map.! startingValve) (timeLimit2 + 1) nonZeroFlowMap elephant nonZeroPaths
          )
          myWith1
          elephantWith1

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
