import System.IO
import Control.Monad
import Data.List
import Text.Regex.TDFA (AllTextMatches(getAllTextMatches), (=~))

readInt :: String -> Int
readInt = read

extractMatches :: String -> [String]
extractMatches str = getAllTextMatches (str =~ "(don't\\(\\))|(do\\(\\))|mul(\\([[:digit:]]+),([[:digit:]]+)\\)" :: AllTextMatches [] String)

extractNumbers :: String -> String
extractNumbers str = str =~ "[[:digit:]]+,[[:digit:]]+" :: String

stringToNumberTuple :: String -> (Int, Int)
stringToNumberTuple s = 
    let (x, y) = span (/= ',') s 
    in (read x, read (tail y))

multiplyTuple :: (Int, Int) -> Int
multiplyTuple (a, b) = a * b

sumDos :: [String] -> (Int, Bool)
sumDos = foldl (\acc x -> 
  if x == "do()" 
    then (fst acc, True) 
  else if x == "don't()"
    then (fst acc, False) 
  else if snd acc 
    then (fst acc + multiplyTuple (stringToNumberTuple (extractNumbers x)), True)
  else 
    acc) 
  (0, True)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let matches = extractMatches contents
  let answer = sumDos matches
  print $ fst answer
