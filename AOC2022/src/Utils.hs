module Utils where

inputFile :: Int -> String
inputFile n = "input/day" ++ show n ++ ".in"

readInt :: String -> Int
readInt = read

type DayInput a = String -> a

type DaySolution a = a -> Int

type DayMain = String -> IO Int

standardMain :: DaySolution a -> DayInput a -> DayMain
standardMain solution inputReader s = return $ (solution . inputReader) s
