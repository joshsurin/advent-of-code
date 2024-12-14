import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.List
import Data.Function
import Control.Monad (foldM)

type HashMap = HM.HashMap

readInt :: String -> Int
readInt = read

processArray :: [String] -> HashMap Int [Int]
processArray = foldr insertIntoMap HM.empty
  where
    insertIntoMap str hm =
      let (key, rest) = span (/= '|') str
          value = read (tail rest) :: Int
          k = read key :: Int
      in HM.insertWith (++) k [value] hm

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
                  "" -> []
                  s' -> w : splitOn p s''
                        where (w, s'') = break p s'

processUpdate :: String -> [Int]
processUpdate str = map readInt (splitOn (==',') str)

isGoodSingleUpdate :: Int -> [Int] -> HM.HashMap Int [Int] -> Bool
isGoodSingleUpdate x values hm = 
  case HM.lookup x hm of
    Nothing -> True
    Just hmValues -> not (any (`elem` hmValues) values)

isGoodUpdate :: [Int] -> HM.HashMap Int [Int] -> Bool
isGoodUpdate [] _ = True
isGoodUpdate (x:xs) hm = 
  all (\(y, ys) -> isGoodSingleUpdate y ys hm) (zip xs (tail (inits (x:xs))))
  where
    inits [] = [[]]
    inits ys = [] : [take n ys | n <- [1..length ys]]

middleValue :: [Int] -> Int
middleValue [] = error "no middle"
middleValue xs = xs !! middleIndex
  where
    len = length xs
    middleIndex = len `div` 2

specialCompare :: Int -> Int -> HashMap Int [Int] -> Ordering
specialCompare x y hm = if y `elem` (indexHashMap x hm) 
                        then LT 
                        else if x `elem` (indexHashMap y hm)
                        then GT
                        else EQ
          
indexHashMap :: Int -> HashMap Int [Int] -> [Int]
indexHashMap x hm = case HM.lookup x hm of
                    Nothing -> []
                    Just hmValues -> hmValues

specialSort :: [Int] -> HashMap Int [Int] -> [Int]
specialSort xs hm = sortBy (\ x y -> specialCompare x y hm) xs

main :: IO ()
main = do
  contents <- lines <$> readFile "input.txt"
  let rules = takeWhile (/="") contents
  let updates = tail $ dropWhile (/="") contents
  let rulesHashMap = processArray rules
  let processedUpdates = map processUpdate updates
  -- iterate each value in each updates tail and check it in the hashmap. check it against all previous values, e.g [75,29,13], we check 29 against 75, lookup key 29 and see if 75 exists. if it does, we filter it out. 
  let badUpdates = filter (\u -> not (isGoodUpdate u rulesHashMap)) processedUpdates
  let sortedBadUpdates = map (\x -> specialSort x rulesHashMap) badUpdates
  let answer = sum $ map (\u -> middleValue u) sortedBadUpdates
  print answer
