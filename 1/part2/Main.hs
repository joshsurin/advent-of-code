import System.IO
import Control.Monad
import Data.List

readInt :: String -> Int
readInt = read

splitEvery2 :: [a] -> ([a], [a])
splitEvery2 [] = ([], [])
splitEvery2 (x:y:xs) = (x:xs1, y:xs2)
  where (xs1, xs2) = splitEvery2 xs

getSimilarityScore :: Int -> [Int] -> Int
getSimilarityScore x ys = sum [ y | y <- ys, y == x ]

main = do
  contents <- readFile "input.txt"
  let splitListTuple = splitEvery2 $ map readInt $ words contents
  let (leftList, rightList) = splitListTuple
  let similarityScores = map (`getSimilarityScore` rightList) leftList
  let answer = sum similarityScores
  print answer

