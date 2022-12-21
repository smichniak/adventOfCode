module Day21 where

import qualified Data.Map as Map (Map, fromList, insert, (!))
import Data.Maybe (fromJust)
import Text.Parsec (count, letter, many, oneOf, (<|>))
import Text.Parsec.String (Parser)
import Utils (DayInput, DayMain, DaySolution, intParser, parseString, standardMain)

data MonkeyJob = Num Int | Add String String | Sub String String | Mul String String | Div String String

type MonkeyMap = Map.Map String MonkeyJob

type InputType = MonkeyMap

makeJob :: Char -> String -> String -> MonkeyJob
makeJob '+' = Add
makeJob '-' = Sub
makeJob '*' = Mul
makeJob '/' = Div

jobParser :: Parser MonkeyJob
jobParser = Num <$> intParser <|> flip makeJob <$> (many letter <* oneOf " ") <*> oneOf "+-*/" <*> (oneOf " " *> many letter)

monkeyParser :: Parser (String, MonkeyJob)
monkeyParser = (,) <$> (many letter <* count 2 (oneOf ": ")) <*> jobParser

inputParser :: DayInput InputType
inputParser = Map.fromList . map (parseString monkeyParser) . lines

getOp :: MonkeyJob -> (Int -> Int -> Int)
getOp (Add _ _) = (+)
getOp (Sub _ _) = (-)
getOp (Mul _ _) = (*)
getOp (Div _ _) = div

getNames :: MonkeyJob -> (String, String)
getNames (Add name1 name2) = (name1, name2)
getNames (Sub name1 name2) = (name1, name2)
getNames (Mul name1 name2) = (name1, name2)
getNames (Div name1 name2) = (name1, name2)

getMonkeyNum :: MonkeyMap -> String -> Int
getMonkeyNum monkeyMap monkeyName =
  case monkeyMap Map.! monkeyName of
    Num num -> num
    job -> op (getMonkeyNum monkeyMap name1) (getMonkeyNum monkeyMap name2)
      where
        op = getOp job
        (name1, name2) = getNames job

rootMonkey :: String
rootMonkey = "root"

solution1 :: DaySolution InputType
solution1 = flip getMonkeyNum rootMonkey

myName :: String
myName = "humn"

getUpperBound :: MonkeyMap -> String -> Int -> Int -> Int
getUpperBound monkeyMap name1 val2 humnVal =
  let val1 = getMonkeyNum (Map.insert myName (Num humnVal) monkeyMap) name1
   in if val1 < val2 then humnVal else getUpperBound monkeyMap name1 val2 (2 * humnVal)

binsearch :: MonkeyMap -> String -> Int -> Int -> Int -> Int
binsearch monkeyMap name1 target left right
  | left == right = left
  | midVal > target = binsearch monkeyMap name1 target (mid + 1) right
  | otherwise = binsearch monkeyMap name1 target left mid
  where
    mid = left + ((right - left) `div` 2)
    newMap = Map.insert myName (Num mid) monkeyMap
    midVal = getMonkeyNum newMap name1

-- It can be observed that the value for second name does not depend on the "humn" value and the first one is monotonic, we can can binserach it
solution2 :: DaySolution InputType
solution2 monkeyMap =
  let (name1, name2) = getNames $ monkeyMap Map.! rootMonkey
      val2 = getMonkeyNum monkeyMap name2
      upperBound = getUpperBound monkeyMap name1 val2 1
   in binsearch monkeyMap name1 val2 1 upperBound

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
