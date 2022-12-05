module Utils where

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
