module Day19 where

import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as Seq (Seq (Empty), singleton)
import Text.Parsec (between, endBy, many)
import Text.Parsec.String (Parser)
import Utils (DayInput, DayMain, DaySolution, intParser, notNumber, parseString, standardMain)

import Control.Parallel.Strategies (using, parList, rdeepseq)

data Blueprint = BP {oreCost :: Int, clayCost :: Int, obsydianCost :: (Int, Int), geodeCost :: (Int, Int)} deriving (Show)

type InputType = [Blueprint]

data State = ST {timeLeft :: Int, ore :: Int, clay :: Int, obsydian :: Int, geodes :: Int, oreRobots :: Int, clayRobots :: Int, obsydianRobots :: Int, geodeRobots :: Int} deriving (Eq, Ord)

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

-- openEvalState :: Blueprint -> OpenFunction State Int
-- openEvalState _ self (ST 0 _ _ _ geodeNum _ _ _ _) = return geodeNum
-- openEvalState
--   (BP oreRobotCost clayRobotCost (obsOreCost, obsClayCost) (geodeOreCost, geodeObsCost))
--   self
--   (ST time oreNum clayNum obsydianNum geodeNum oreRobotNum clayRobotNum obsydianRobotNum geodeRobotNum) = do
--     buildOre <- if oreNum - oreRobotCost >= 0 then self (ST (time - 1) (oreNum + oreRobotNum - oreRobotCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) (oreRobotNum + 1) clayRobotNum obsydianRobotNum geodeRobotNum) else return 0
--     buildClay <- if oreNum - clayRobotCost >= 0 then self (ST (time - 1) (oreNum + oreRobotNum - clayRobotCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum (clayRobotNum + 1) obsydianRobotNum geodeRobotNum) else return 0
--     buildObs <- if oreNum - obsOreCost >= 0 && clayNum - obsClayCost >= 0 then self (ST (time - 1) (oreNum + oreRobotNum - obsOreCost) (clayNum + clayRobotNum - obsClayCost) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum (obsydianRobotNum + 1) geodeRobotNum) else return 0
--     buildGeode <- if oreNum - geodeOreCost >= 0 && obsydianNum - geodeObsCost >= 0 then self (ST (time - 1) (oreNum + oreRobotNum - geodeOreCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum - geodeObsCost) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum obsydianRobotNum (geodeRobotNum + 1)) else return 0
--     noBuild <- self (ST (time - 1) (oreNum + oreRobotNum) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum obsydianRobotNum geodeRobotNum)

--     return $ maximum [buildOre, buildClay, buildObs, buildGeode, noBuild]

-- evalState :: Blueprint -> State -> Int
-- evalState _ (ST 0 _ _ _ geodeNum _ _ _ _) =  geodeNum
-- evalState
--   bp@(BP oreRobotCost clayRobotCost (obsOreCost, obsClayCost) (geodeOreCost, geodeObsCost))
--   (ST time oreNum clayNum obsydianNum geodeNum oreRobotNum clayRobotNum obsydianRobotNum geodeRobotNum) = let
--     buildGeode = if oreNum - geodeOreCost >= 0 && obsydianNum - geodeObsCost >= 0 then evalState bp (ST (time - 1) (oreNum + oreRobotNum - geodeOreCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum - geodeObsCost) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum obsydianRobotNum (geodeRobotNum + 1)) else 0
--     buildObs = if oreNum - obsOreCost >= 0 && clayNum - obsClayCost >= 0 then evalState bp (ST (time - 1) (oreNum + oreRobotNum - obsOreCost) (clayNum + clayRobotNum - obsClayCost) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum (obsydianRobotNum + 1) geodeRobotNum) else 0
--     buildClay = if oreNum - clayRobotCost >= 0 then evalState bp (ST (time - 1) (oreNum + oreRobotNum - clayRobotCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum (clayRobotNum + 1) obsydianRobotNum geodeRobotNum) else 0
--     buildOre = if oreNum - oreRobotCost >= 0 then evalState bp (ST (time - 1) (oreNum + oreRobotNum - oreRobotCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) (oreRobotNum + 1) clayRobotNum obsydianRobotNum geodeRobotNum) else 0
--     noBuild = evalState bp (ST (time - 1) (oreNum + oreRobotNum) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum obsydianRobotNum geodeRobotNum)

--     in  maximum [buildOre, buildClay, buildObs, buildGeode, noBuild]

-- evalState :: Blueprint -> State -> Int
-- evalState _ (ST 0 _ _ _ geodeNum _ _ _ _) = geodeNum
-- evalState
--   bp@(BP oreRobotCost clayRobotCost (obsOreCost, obsClayCost) (geodeOreCost, geodeObsCost))
--   (ST time oreNum clayNum obsydianNum geodeNum oreRobotNum clayRobotNum obsydianRobotNum geodeRobotNum)
--     | oreNum - geodeOreCost >= 0 && obsydianNum - geodeObsCost >= 0 = evalState bp (ST (time - 1) (oreNum + oreRobotNum - geodeOreCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum - geodeObsCost) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum obsydianRobotNum (geodeRobotNum + 1))
--     | otherwise =
--         let buildObs = if oreNum - obsOreCost >= 0 && clayNum - obsClayCost >= 0 then evalState bp (ST (time - 1) (oreNum + oreRobotNum - obsOreCost) (clayNum + clayRobotNum - obsClayCost) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum (obsydianRobotNum + 1) geodeRobotNum) else 0
--             buildClay = if oreNum - clayRobotCost >= 0 then evalState bp (ST (time - 1) (oreNum + oreRobotNum - clayRobotCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum (clayRobotNum + 1) obsydianRobotNum geodeRobotNum) else 0
--             buildOre = if oreNum - oreRobotCost >= 0 then evalState bp (ST (time - 1) (oreNum + oreRobotNum - oreRobotCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) (oreRobotNum + 1) clayRobotNum obsydianRobotNum geodeRobotNum) else 0
--             noBuild = evalState bp (ST (time - 1) (oreNum + oreRobotNum) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum obsydianRobotNum geodeRobotNum)
--          in maximum [buildObs, buildClay, buildOre, noBuild]

-- evalState :: Blueprint -> Seq State -> Int -> Int
-- evalState bp Seq.Empty acc = acc
-- evalState
--   bp@(BP oreRobotCost clayRobotCost (obsOreCost, obsClayCost) (geodeOreCost, geodeObsCost))
--   (ST time oreNum clayNum obsydianNum geodeNum oreRobotNum clayRobotNum obsydianRobotNum geodeRobotNum :<| rest)
--   acc =
--     let garbageState = ST 0 0 0 0 0 0 0 0 0
--         addGeodeState = if oreNum - geodeOreCost >= 0 && obsydianNum - geodeObsCost >= 0 then ST (time - 1) (oreNum + oreRobotNum - geodeOreCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum - geodeObsCost) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum obsydianRobotNum (geodeRobotNum + 1) else garbageState
--         addObs = if oreNum - obsOreCost >= 0 && clayNum - obsClayCost >= 0 then ST (time - 1) (oreNum + oreRobotNum - obsOreCost) (clayNum + clayRobotNum - obsClayCost) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum (obsydianRobotNum + 1) geodeRobotNum else garbageState
--         addClay = if oreNum - clayRobotCost >= 0 then ST (time - 1) (oreNum + oreRobotNum - clayRobotCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum (clayRobotNum + 1) obsydianRobotNum geodeRobotNum else garbageState
--         addOre = if oreNum - oreRobotCost >= 0 then ST (time - 1) (oreNum + oreRobotNum - oreRobotCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) (oreRobotNum + 1) clayRobotNum obsydianRobotNum geodeRobotNum else garbageState
--         noBuild = ST (time - 1) (oreNum + oreRobotNum) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum obsydianRobotNum geodeRobotNum
--         newSeq = addGeodeState :<| addObs :<| addClay :<| addOre :<| noBuild :<| rest
--      in if time == 0 then evalState bp rest (max acc geodeNum) else evalState bp newSeq acc

evalState :: Blueprint -> Seq State -> Int -> Int
evalState bp Seq.Empty acc = acc
evalState
  bp@(BP oreRobotCost clayRobotCost (obsOreCost, obsClayCost) (geodeOreCost, geodeObsCost))
  (ST time oreNum clayNum obsydianNum geodeNum oreRobotNum clayRobotNum obsydianRobotNum geodeRobotNum :<| rest)
  acc =
    let garbageState = ST 0 0 0 0 0 0 0 0 0
        addGeodeState = if oreNum - geodeOreCost >= 0 && obsydianNum - geodeObsCost >= 0 then ST (time - 1) (oreNum + oreRobotNum - geodeOreCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum - geodeObsCost) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum obsydianRobotNum (geodeRobotNum + 1) else garbageState
        addObs = if oreNum - obsOreCost >= 0 && clayNum - obsClayCost >= 0 then ST (time - 1) (oreNum + oreRobotNum - obsOreCost) (clayNum + clayRobotNum - obsClayCost) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum (obsydianRobotNum + 1) geodeRobotNum else garbageState
        addClay = if oreNum - clayRobotCost >= 0 then ST (time - 1) (oreNum + oreRobotNum - clayRobotCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum (clayRobotNum + 1) obsydianRobotNum geodeRobotNum else garbageState
        addOre = if oreNum - oreRobotCost >= 0 then ST (time - 1) (oreNum + oreRobotNum - oreRobotCost) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) (oreRobotNum + 1) clayRobotNum obsydianRobotNum geodeRobotNum else garbageState
        noBuild = ST (time - 1) (oreNum + oreRobotNum) (clayNum + clayRobotNum) (obsydianNum + obsydianRobotNum) (geodeNum + geodeRobotNum) oreRobotNum clayRobotNum obsydianRobotNum geodeRobotNum
        newSeq = addObs :<| addClay :<| addOre :<| noBuild :<| rest
     in if time == 0 || 2 * geodeNum + time * (2 * geodeRobotNum + time - 1) < 2 * acc
          then evalState bp rest (max acc geodeNum)
          else
            if oreNum - geodeOreCost >= 0 && obsydianNum - geodeObsCost >= 0
              then evalState bp (addGeodeState :<| rest) acc
              else evalState bp newSeq acc

initialState :: State
initialState = ST maxTime 0 0 0 0 1 0 0 0

evalBlueprint :: Blueprint -> Int
-- evalBlueprint bp = memoized (openEvalState bp) initialState
evalBlueprint bp = evalState bp (Seq.singleton initialState) 0

solution1 :: DaySolution InputType
solution1 bps = 
  let bpScores = zipWith (\num -> (* num) . evalBlueprint) [1 ..] bps
      parScores = bpScores `using` parList rdeepseq
    in sum parScores

solution2 :: DaySolution InputType
solution2 = undefined

main1 :: DayMain -- 976 too low
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
