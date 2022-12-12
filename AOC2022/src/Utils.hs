module Utils where

import qualified Data.Set as Set

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

compose :: Int -> (a -> a) -> a -> a
compose n f = foldr (.) id (replicate n f)

dx :: [Int]
dx = [0, 0, -1, 1]

dy :: [Int]
dy = [-1, 1, 0, 0]
