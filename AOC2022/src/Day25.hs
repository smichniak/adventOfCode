module Day25 where

import Utils (DayInput, DayMain, DayStringSolution, standardStringMain)

data SNAFUDigit = DoubleMinus | Minus | Zero | One | Two

instance Show SNAFUDigit where
  show DoubleMinus = "="
  show Minus = "-"
  show Zero = "0"
  show One = "1"
  show Two = "2"

type SNAFUNumber = [SNAFUDigit]

type InputType = [SNAFUNumber]

getDigit :: Char -> SNAFUDigit
getDigit '=' = DoubleMinus
getDigit '-' = Minus
getDigit '0' = Zero
getDigit '1' = One
getDigit '2' = Two

inputParser :: DayInput InputType
inputParser s = map (map getDigit . reverse) (lines s)

digitToNum :: SNAFUDigit -> Int
digitToNum DoubleMinus = -2
digitToNum Minus = -1
digitToNum Zero = 0
digitToNum One = 1
digitToNum Two = 2

base :: Int
base = 5

snafuToDecimal :: SNAFUNumber -> Int
snafuToDecimal [] = 0
snafuToDecimal (digit : rest) = digitToNum digit + base * snafuToDecimal rest

digitOrder :: [SNAFUDigit]
digitOrder = [Zero, One, Two, DoubleMinus, Minus]

decimalToSnafu :: Int -> SNAFUNumber
decimalToSnafu 0 = []
decimalToSnafu num =
  let firstDigit = digitOrder !! (num `mod` base)
   in firstDigit : decimalToSnafu ((num - digitToNum firstDigit) `div` base)

showSnafu :: SNAFUNumber -> String
showSnafu = concatMap show . reverse

solution1 :: DayStringSolution InputType
solution1 = showSnafu . decimalToSnafu . sum . map snafuToDecimal

main1 :: DayMain
main1 = standardStringMain solution1 inputParser

main2 :: DayMain
main2 _ = return "No Part 2 for Day 25"
