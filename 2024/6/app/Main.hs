import Data.List (elemIndex)
import qualified Data.Set as Set 

position :: Eq a => a -> [a] -> Int
position x xs = case elemIndex x xs of
    Just index -> index
    Nothing    -> -1

getGuardPosition :: [String] -> (Int, Int)
getGuardPosition = findGuardPosition 0
  where
    findGuardPosition _ [] = (-1, -1)
    findGuardPosition rowIndex (row:rows) =
        let columnIndex = position '^' row
        in if columnIndex /= -1
           then (rowIndex, columnIndex)
           else findGuardPosition (rowIndex + 1) rows

-- directions
-- 1 = ^
-- 2 = >
-- 3 = v
-- 4 = <
traverseMap :: (Int, Int) -> Int -> [String] -> Set.Set (Int, Int) -> Set.Set (Int, Int)
traverseMap (x, y) d xs m
    | x >= length xs || y >= length (xs !! x) || x < 0 || y < 0 = m
    | xs !! x !! y == '#' = traverseMap previousPosition direction xs m
    | otherwise = traverseMap nextPosition d xs (Set.insert (x, y) m)
    where
        previousPosition = case d of
            1 -> (x + 1, y)
            2 -> (x, y - 1)
            3 -> (x - 1, y)
            4 -> (x, y + 1)
            _ -> (x, y)
        direction = if d == 4 then 1 else d + 1
        nextPosition = case d of
            1 -> (x - 1, y)
            2 -> (x, y + 1)
            3 -> (x + 1, y)
            4 -> (x, y - 1)
            _ -> (x, y)

main :: IO ()
main = do
  contents <- lines <$> readFile "input.txt"
  let startingGuardPosition = getGuardPosition contents
  let answer = length $ traverseMap startingGuardPosition 1 contents Set.empty
  print answer  
