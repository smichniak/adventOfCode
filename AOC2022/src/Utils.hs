module Utils where

import qualified Data.Set as Set
import Text.Parsec (char, digit, eof, many1, noneOf, option, parse)
import Text.Parsec.String (Parser)

fromBool :: Bool -> Int
fromBool True = 1
fromBool False = 0

inputFile :: Int -> String
inputFile n = "input/day" ++ show n ++ ".in"

readInt :: String -> Int
readInt = read

type DayInput a = String -> a

type DaySolution a = a -> Int

type DayStringSolution a = a -> String

type DayMain = String -> IO String

standardMain :: DaySolution a -> DayInput a -> DayMain
standardMain solution inputReader s = return $ show $ (solution . inputReader) s

standardStringMain :: DayStringSolution a -> DayInput a -> DayMain
standardStringMain solution inputReader s = return $ (solution . inputReader) s

partitions :: [a] -> [([a], [a])]
-- partitions l = foldl (\acc num -> splitAt num l : acc) [] [0 .. length l]
partitions l = foldl (flip $ (:) . flip splitAt l) [] [0 .. length l]

condInsert :: Ord a => Bool -> a -> Set.Set a -> Set.Set a
condInsert True x = Set.insert x
condInsert False _ = id

combine :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
combine g f x = g . f x

butLast :: [a] -> a
butLast [x, _] = x
butLast (_ : xs) = butLast xs

getNext :: (Eq a) => a -> [a] -> Maybe a
getNext x l = lookup x $ (zip <*> tail) $ cycle l

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

compose :: Int -> (a -> a) -> a -> a
compose n f = foldr (.) id (replicate n f)

dx :: [Int]
dx = [0, 0, -1, 1]

dy :: [Int]
dy = [-1, 1, 0, 0]

take2 :: [a] -> (a, a)
take2 (x : y : _) = (x, y)

take3 :: [a] -> (a, a, a)
take3 (x : y : z : _) = (x, y, z)

parseString :: Parser a -> String -> a
parseString par str =
  let Right result = parse (par <* eof) "" str
   in result

natParser :: Parser Int
natParser = readInt <$> many1 digit

sign :: Num a => Parser (a -> a)
sign = option id (char '-' >> return negate)

intParser :: Parser Int
intParser = sign <*> natParser

notNumber :: Parser Char
notNumber =  noneOf ('-' : ['0' .. '9'])

notUpper :: Parser Char
notUpper = noneOf ['A' .. 'Z']
