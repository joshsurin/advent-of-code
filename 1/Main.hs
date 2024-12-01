import System.IO
import Control.Monad
import Data.List

readInt :: String -> Int
readInt = read

splitEvery2 :: [a] -> ([a], [a])
splitEvery2 [] = ([], [])
splitEvery2 (x:y:xs) = (x:xs1, y:xs2)
  where (xs1, xs2) = splitEvery2 xs

mapBoth :: (a -> b) -> (a, a) -> (b , b)
mapBoth f (a, b) = (f a, f b)

diffTuple :: (Int, Int) -> Int
diffTuple (a, b) = abs (a - b)

main = do
  contents <- readFile "input.txt"
  let answer = sum $ map diffTuple $ uncurry zip . mapBoth sort $ splitEvery2 $ map readInt $ words contents
  print answer

