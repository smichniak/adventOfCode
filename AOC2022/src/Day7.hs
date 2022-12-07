module Day7 where

import Data.Foldable (Foldable (toList))
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Utils (DayInput, DayMain, DaySolution, readInt, standardMain)

type FileName = String

type Path = [FileName]

type FileSize = Int

data File = File FileName FileSize | Dir FileName deriving (Show)

type FileMap = Map.Map Path [File]

data FileTreeGeneric a
  = FileNode {filename :: FileName, fileSize :: a}
  | DirNode {filename :: FileName, fileSize :: a, fileTree :: [FileTreeGeneric a]}
  deriving (Show)

type FileTree = FileTreeGeneric FileSize

instance Foldable FileTreeGeneric where
  foldr = foldTree

type InputType = FileTree

up :: String
up = ".."

maxSize :: Int
maxSize = 100000

minFree :: Int
minFree = 30000000

total :: Int
total = 70000000

foldTree :: (a -> b -> b) -> b -> FileTreeGeneric a -> b
foldTree fn base (FileNode _ fileSize) = base
foldTree fn base (DirNode _ fileSize []) = fn fileSize base
foldTree fn base (DirNode name size (child : rest)) = foldTree fn (foldTree fn base child) (DirNode name size rest)

getLs :: [[String]] -> [File]
getLs [] = []
getLs (["dir", dirName] : restResults) = Dir dirName : getLs restResults
getLs ([fileSize, fileName] : restResults) = File fileName (readInt fileSize) : getLs restResults

buildDirs :: [FileName] -> [[String]] -> FileMap
buildDirs _ [] = Map.empty
buildDirs currentDir ([cd] : rest) =
  let newDir = last $ splitOn " " cd
   in if newDir == up
        then buildDirs (tail currentDir) rest
        else buildDirs (newDir : currentDir) rest
buildDirs currentDir (ls : rest) =
  let restMap = buildDirs currentDir rest
      lsCommand = map (splitOn " ") (tail ls)
      fileList = getLs lsCommand
   in Map.insert currentDir fileList restMap

getFileTree :: FileMap -> Path -> File -> FileTree
getFileTree _ _ (File fileName fileSize) = FileNode fileName fileSize
getFileTree fileMap path (Dir dirName) =
  let children = treeBuilder (dirName : path) fileMap
   in DirNode dirName (sum $ map fileSize children) children

parser :: String -> FileMap
parser s = buildDirs [] (map lines (tail $ splitOn "$" s))

treeBuilder :: Path -> FileMap -> [FileTree]
treeBuilder path fileMap = map (getFileTree fileMap path) (fileMap Map.! path)

inputParser :: DayInput InputType
inputParser s = getFileTree (parser s) [] (Dir "/")

solution1 :: DaySolution InputType
solution1 t = sum $ filter (<= maxSize) $ toList t

solution2 :: DaySolution InputType
solution2 t = minimum $ filter (>= minFree - total + fileSize t) $ toList t

main1 :: DayMain
main1 = standardMain solution1 inputParser

main2 :: DayMain
main2 = standardMain solution2 inputParser
