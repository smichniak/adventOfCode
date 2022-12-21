module Day20 where

import Data.Bifunctor (second)
import Data.CircularList (CList, findRotateTo, focus, fromList, insertR, removeR, rotN)
import Data.Maybe (fromJust)
import Utils (DayInput, DayMain, DaySolution, compose, readInt, standardMain)

type IndexedValue = (Int, Int)

type InputType = [IndexedValue]

inputParser :: DayInput InputType
inputParser = zip [1 ..] . map readInt . lines

mixItem :: Int -> CList IndexedValue -> Int -> CList IndexedValue
mixItem len list index =
  let itemInList = fromJust $ findRotateTo ((== index) . fst) list
      (_, item) = fromJust $ focus itemInList
      itemRemoved = removeR itemInList
      rotated = rotN (item `mod` (len - 1)) itemRemoved
   in insertR (index, item) rotated

mixList :: Int -> [Int] -> CList IndexedValue -> CList IndexedValue
mixList len itemOrder list = foldl (mixItem len) list itemOrder

findAfterItem :: Int -> CList IndexedValue -> Int -> Int
findAfterItem item list distance = snd $ fromJust $ focus $ rotN distance (fromJust $ findRotateTo ((== item) . snd) list)

coordinateIndices :: [Int]
coordinateIndices = [1000, 2000, 3000]

solution1 :: DaySolution InputType
solution1 items =
  let cList = fromList items
      mixed = mixList (length items) (map fst items) cList
   in sum $ map (findAfterItem 0 mixed) coordinateIndices

decryptionKey :: Int
decryptionKey = 811589153

mixNum :: Int
mixNum = 10

solution2 :: DaySolution InputType
solution2 items =
  let decrypted = map (second (* decryptionKey)) items
      cList = fromList decrypted
      mixed = compose mixNum (mixList (length items) (map fst items)) cList
   in sum $ map (findAfterItem 0 mixed) coordinateIndices

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
