import Data.List (elemIndex)
import qualified Data.Set as Set 
import Data.List (groupBy, sortOn)
import Data.Function (on)

position :: Eq a => a -> [a] -> Int
position x xs = case elemIndex x xs of
    Just index -> index
    Nothing    -> -1


getAntennaSet :: [String] -> Set.Set (Char, [(Int, Int)])
getAntennaSet xs = Set.fromList groupedIndices
  where
    indexedChars = concatMap (\(row, str) -> 
                      [(char, (row, col)) | (col, char) <- zip [0..] str, char /= '.']
                    ) (zip [0..] xs)
    groupedIndices = map (\group -> (fst (head group), map snd group)) 
                      $ groupBy ((==) `on` fst) 
                      $ sortOn fst indexedChars

generateUniquePairs :: [(Int, Int)] -> [[(Int, Int)]]
generateUniquePairs lst = [[x, y] | (x, i) <- zip lst [0..], (y, j) <- zip lst [0..], i < j]

getAntennaSetPairs :: Set.Set (Char, [(Int, Int)]) -> Set.Set (Char, [[(Int, Int)]])
getAntennaSetPairs = Set.map (\(char, positions) -> (char, generateUniquePairs positions))

-- [(1,8), (2,5)] -> [(0, 11), (3, 2] (if within bounds)
getAntinodes :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
getAntinodes xs x y = 
    let rightMostX = max (fst (xs !! 0)) (fst (xs !! 1))
        upMostY = min (snd (xs !! 0)) (snd (xs !! 1))
        leftMostX = min (fst (xs !! 0)) (fst (xs !! 1))
        downMostY = max (snd (xs !! 0)) (snd (xs !! 1))
        diffX = abs (fst (xs !! 0) - fst (xs !! 1))
        diffY = abs (snd (xs !! 0) - snd (xs !! 1))
        (firstAntinode, secondAntinode) = 
            if fst (xs !! 0) < fst (xs !! 1) && snd (xs !! 0) < snd (xs !! 1)
            then 
                let firstAntinode = (rightMostX + diffX, downMostY + diffY)
                    secondAntinode = (leftMostX - diffX, upMostY - diffY)
                in (firstAntinode, secondAntinode)
            else 
                let firstAntinode = (rightMostX + diffX, upMostY - diffY)
                    secondAntinode = (leftMostX - diffX, downMostY + diffY)
                in (firstAntinode, secondAntinode)
        isValid (a, b) = a >= 0 && a <= x && b >= 0 && b <= y
    in filter isValid [firstAntinode, secondAntinode]

getAntinodesSet :: [[(Int, Int)]] -> Int -> Int -> Set.Set (Int, Int) -> Set.Set (Int, Int)
getAntinodesSet [] _ _ acc = acc
getAntinodesSet (x:xs) bx by acc = 
    let antinodes = getAntinodes x bx by
        newAcc = Set.union acc (Set.fromList antinodes)
    in getAntinodesSet xs bx by newAcc

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let antennaSet = getAntennaSet contents
    let antennaSetPairs = getAntennaSetPairs antennaSet
    let l = length contents - 1
    let ly = length (contents !! 0) - 1
    let resultSet = foldr (\(_, lst) acc -> getAntinodesSet lst l ly acc) Set.empty antennaSetPairs
    print $ length resultSet
