module Solutions where

import Utils (DayMain)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

solutions :: [DayMain]
solutions = [Day1.main1, Day1.main2,
             Day2.main1, Day2.main2,
             Day3.main1, Day3.main2,
             Day4.main1, Day4.main2]
