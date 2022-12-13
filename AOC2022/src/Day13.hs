module Day13 where

import Control.Applicative ((<|>))
import Data.List (elemIndex, findIndices, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)
import Text.Parsec (between, char, digit, eof, many1, parse, sepBy)
import Text.Parsec.String (Parser)
import Utils (DayInput, DayMain, DaySolution, readInt, standardMain, take2)

type InputType = [(Nested, Nested)]

data Nested = N [Nested] | Num Int deriving (Eq)

instance Ord Nested where
  nested1 <= nested2 = fromJust $ compareNested nested1 nested2

nested :: Parser Nested
nested = Num <$> (read <$> many1 digit) <|> N <$> between (char '[') (char ']') (nested `sepBy` char ',')

parseNested :: String -> Nested
parseNested str =
  let Right result = parse (nested <* eof) "" str
   in result

inputParser :: DayInput InputType
inputParser s = map (take2 . map parseNested . lines) (splitOn "\n\n" s)

lengthCompare :: [Nested] -> [Nested] -> Maybe Bool
lengthCompare l1 l2 =
  let len1 = length l1
      len2 = length l2
   in if len1 == len2 then Nothing else Just (len1 < len2)

compareNested :: Nested -> Nested -> Maybe Bool
compareNested (Num n1) (Num n2)
  | n1 == n2 = Nothing
  | otherwise = Just (n1 < n2)
compareNested (N l1) (N l2) =
  let compareElements = foldl (<|>) Nothing $ zipWith compareNested l1 l2
   in compareElements <|> lengthCompare l1 l2
compareNested (Num n1) n = compareNested (N [Num n1]) n
compareNested n (Num n2) = compareNested n (N [Num n2])

solution1 :: DaySolution InputType
solution1 = sum . map (+ 1) . findIndices id . mapMaybe (uncurry compareNested)

divider1 :: Nested
divider1 = parseNested "[[2]]"

divider2 :: Nested
divider2 = parseNested "[[6]]"

solution2 :: DaySolution InputType
solution2 nestedList =
  let packets = divider1 : divider2 : concatMap (\(x, y) -> [x, y]) nestedList
      sorted = sort packets
   in product $ map (+ 1) $ findIndices (`elem` [divider1, divider2]) sorted

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
