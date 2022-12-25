module Day19 where

import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as Seq (Seq (Empty), singleton)
import qualified Data.Set as Set
import Text.Parsec (between, endBy, many)
import Text.Parsec.String (Parser)
import Utils (DayInput, DayMain, DaySolution, intParser, notNumber, parseString, standardMain)

data Blueprint = BP {oreCost :: Int, clayCost :: Int, obsydianCost :: (Int, Int), geodeCost :: (Int, Int)}

type InputType = [Blueprint]

data State = ST
  { timeLeft :: Int,
    ore :: Int,
    clay :: Int,
    obsydian :: Int,
    geodes :: Int,
    oreRobots :: Int,
    clayRobots :: Int,
    obsydianRobots :: Int,
    geodeRobots :: Int
  }
  deriving (Eq, Ord)

maxTime :: Int
maxTime = 24

lineParser :: Parser [Int]
lineParser = between (many notNumber) (many notNumber) (endBy intParser (many notNumber))

inputParser :: DayInput InputType
inputParser =
  map
    ( ( \(_ : oreCostVal : clayCostVal : obsydianOreCost : obsydianClayCost : geodeOreCost : geodeObsydianCost : _) ->
          BP oreCostVal clayCostVal (obsydianOreCost, obsydianClayCost) (geodeOreCost, geodeObsydianCost)
      )
        . parseString lineParser
    )
    . lines

evalState :: Blueprint -> Seq State -> Set.Set State -> Int -> Int
evalState _ Seq.Empty _ acc = acc
evalState
  bp@(BP oreRobotCost clayRobotCost (obsOreCost, obsClayCost) (geodeOreCost, geodeObsCost))
  (st@(ST time oreNum clayNum obsydianNum geodeNum oreRobotNum clayRobotNum obsydianRobotNum geodeRobotNum) :<| rest)
  visited
  acc =
    let maxOreCost = maximum [oreRobotCost, clayRobotCost, obsOreCost, geodeOreCost]
        garbageState = ST 0 0 0 0 0 0 0 0 0
        addGeodeState = if oreNum - geodeOreCost >= 0 && obsydianNum - geodeObsCost >= 0 then ST (time - 1) (oreNum + oreRobotNum - geodeOreCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum - geodeObsCost) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum obsydianRobotNum (geodeRobotNum + 1) else garbageState
        addObs = if obsydianRobotNum < geodeObsCost && oreNum - obsOreCost >= 0 && clayNum - obsClayCost >= 0 then ST (time - 1) (oreNum + oreRobotNum - obsOreCost) (clayNum + clayRobotNum - obsClayCost) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum (obsydianRobotNum + 1) geodeRobotNum else garbageState
        addClay = if clayRobotNum < obsClayCost && oreNum - clayRobotCost >= 0 then ST (time - 1) (oreNum + oreRobotNum - clayRobotCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum (clayRobotNum + 1) obsydianRobotNum geodeRobotNum else garbageState
        addOre = if oreRobotNum < maxOreCost && oreNum - oreRobotCost >= 0 then ST (time - 1) (oreNum + oreRobotNum - oreRobotCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) (oreRobotNum + 1) clayRobotNum obsydianRobotNum geodeRobotNum else garbageState
        noBuild = ST (time - 1) (oreNum + oreRobotNum) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum obsydianRobotNum geodeRobotNum
        newSeq = addObs :<| addClay :<| addOre :<| noBuild :<| rest
        newVisited = Set.insert st visited
     in if time == 0 || 2 * geodeNum + time * (2 * geodeRobotNum + time - 1) < 2 * acc || Set.member st visited
          then evalState bp rest visited (max acc geodeNum)
          else
            if oreNum - geodeOreCost >= 0 && obsydianNum - geodeObsCost >= 0
              then evalState bp (addGeodeState :<| rest) newVisited acc
              else evalState bp newSeq newVisited acc

initialState :: Int -> State
initialState timeLeft = ST timeLeft 0 0 0 0 1 0 0 0

evalBlueprint :: Int -> Blueprint -> Int
evalBlueprint timeLeft bp = evalState bp (Seq.singleton $ initialState timeLeft) Set.empty 0

solution1 :: DaySolution InputType
solution1 bps =
  let bpScores = zipWith (\num -> (* num) . evalBlueprint maxTime) [1 ..] bps
      parScores = bpScores `using` parList rdeepseq
   in sum parScores

maxTime2 :: Int
maxTime2 = 32

solution2 :: DaySolution InputType
solution2 bps =
  let bpScores = map (evalBlueprint maxTime2) (take 3 bps)
      parScores = bpScores `using` parList rdeepseq
   in product parScores

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
