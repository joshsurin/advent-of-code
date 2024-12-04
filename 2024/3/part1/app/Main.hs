import System.IO
import Control.Monad
import Data.List
import Text.Regex.TDFA (AllTextMatches(getAllTextMatches), (=~))

readInt :: String -> Int
readInt = read

mapBoth :: (a -> b) -> (a, a) -> (b , b)
mapBoth f (a, b) = (f a, f b)

extractMatches :: String -> [String]
extractMatches str = getAllTextMatches (str =~ "mul\\([[:digit:]]+,[[:digit:]]+\\)" :: AllTextMatches [] String)

extractNumbers :: String -> String
extractNumbers str = str =~ "[[:digit:]]+,[[:digit:]]+" :: String

stringTupleToNumber :: (String, String) -> (Int, Int)
stringTupleToNumber (a, b) = (readInt a, readInt b)

multiplyTuple :: (Int, Int) -> Int
multiplyTuple (a, b) = a * b

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let matches = extractMatches contents
  let numberStringList = map extractNumbers matches
  let numberStringTupleList = map (\s -> let (x, y) = span (/= ',') s in (x, tail y)) numberStringList
  let tupleArray = map stringTupleToNumber numberStringTupleList
  let answer = sum $ map multiplyTuple tupleArray
  print answer
