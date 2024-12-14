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

updateAt :: Int -> a -> [a] -> [a]
updateAt n newVal xs 
    | n < 0 || n >= length xs = xs
    | otherwise = take n xs ++ [newVal] ++ drop (n + 1) xs

updateNestedChar :: Int -> Int -> Char -> [String] -> [String]
updateNestedChar i j newChar xs
    | i < 0 || i >= length xs = xs
    | j < 0 || j >= length (xs !! i) = xs
    | otherwise =
        let oldStr = xs !! i
            newStr = take j oldStr ++ [newChar] ++ drop (j + 1) oldStr
        in updateAt i newStr xs

traverseMapLoopOrNot :: (Int, Int) -> Int -> [String] -> Set.Set (Int, Int, Int) -> (Int, Set.Set (Int, Int, Int))
traverseMapLoopOrNot (x, y) d xs visited
    | x < 0 || y < 0 || x >= length xs || y >= length (xs !! x) = (0, visited)
    | otherwise = 
        let currentState = (x, y, d)
        in if Set.member currentState visited
           then (1, visited)
           else 
               if xs !! x !! y == '#'
               then traverseMapLoopOrNot previousPosition direction xs (Set.insert currentState visited)
               else 
                   let newVisited = Set.insert currentState visited
                   in traverseMapLoopOrNot nextPosition d xs newVisited
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

checkLoopAfterReplace :: [String] -> (Int, Int) -> (Int, Int) -> Bool
checkLoopAfterReplace xs g (row, col) = 
            let modifiedContents = updateNestedChar row col '#' xs
                (loopFound, _) = traverseMapLoopOrNot g 1 modifiedContents Set.empty
            in loopFound == 1

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let startingGuardPosition = getGuardPosition contents
    let answer = sum [if checkLoopAfterReplace contents startingGuardPosition (row, col) then 1 else 0 
                         | row <- [0..length contents - 1]
                         , col <- [0..length (contents !! row) - 1]] :: Int
    print answer
