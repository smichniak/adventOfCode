module Day6 where

import Utils (DayInput, DayMain, DaySolution, standardMain)

type InputType = String

packetMarker :: Int
packetMarker = 4

messageMarker :: Int
messageMarker = 14

inputParser :: DayInput InputType
inputParser = id

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

isUnique :: Eq a => [a] -> Bool
isUnique l = all ((1 ==) . flip count l) l

solution :: Int -> Int -> DaySolution InputType
solution markerLen i s = if isUnique $ take markerLen s then i else solution markerLen (i + 1) (tail s)

main1 :: DayMain
main1 = standardMain (solution packetMarker packetMarker) inputParser

main2 :: DayMain
main2 = standardMain (solution messageMarker messageMarker) inputParser
