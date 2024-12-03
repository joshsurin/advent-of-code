import System.IO
import Control.Monad
import Data.List (sort)

readIntList :: [String] -> [Int]
readIntList = map read

readIntListList :: [[String]] -> [[Int]]
readIntListList = map readIntList

isSortedAscending :: [Int] -> Bool
isSortedAscending xs = and $ zipWith (<) xs (tail xs)

isSortedDescending :: [Int] -> Bool
isSortedDescending xs = and $ zipWith (>) xs (tail xs)

isSafe :: [Int] -> Bool
isSafe xs = (isSortedAscending xs && all (\(x, y) -> (y - x) < 4 && (y - x) > 0) (zip xs (tail xs))) ||
            (isSortedDescending xs && all (\(x, y) -> (x - y) < 4 && (x - y) > 0) (zip xs (tail xs)))

removeAt :: Int -> [a] -> [a]
removeAt idx xs = let (ys, zs) = splitAt idx xs in ys ++ tail zs

iterateAndIsSafe :: [Int] -> Bool
iterateAndIsSafe xs = any (isSafe . (`removeAt` xs)) [0..(length xs - 1)]

main = do
  contents <- readFile "input.txt"
  let splitLines = map words $ lines contents
  let splitLineInts = readIntListList splitLines
  let boolList = map iterateAndIsSafe splitLineInts
  let answer = length [ x | x <- boolList, x ]
  print answer
