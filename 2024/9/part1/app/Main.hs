slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)

readInt :: String -> Int
readInt = read

generateBlockFile :: (Int, Char) -> [Int]
generateBlockFile (a, b)
  | a `mod` 2 == 0 = replicate (readInt [b]) (a `div` 2)
  | otherwise      = replicate (readInt [b]) (-1)

shuffleBlockFile :: [Int] -> [Int]
shuffleBlockFile xs = shuffleBlockFile' xs (reverse xs)

shuffleBlockFile' :: [Int] -> [Int] -> [Int]
shuffleBlockFile' [] _ = []
shuffleBlockFile' (x:xs) stack
  | x == -1   = let (y:ys) = dropWhile (== -1) stack in y : shuffleBlockFile' xs ys
  | otherwise = x : shuffleBlockFile' xs stack

getRightMostBlock :: [Int] -> Int
getRightMostBlock (x:xs)
  | x == -1   = getRightMostBlock xs
  | otherwise = x

calculateCheckSum :: (Int, Int) -> Int
calculateCheckSum (a, b) = a * b

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let block = concatMap generateBlockFile (zip [0..] $ input !! 0)
    let ln = length [x | x <- block, x /= -1]
    let shuffledBlock = slice 0 ln $ shuffleBlockFile block
    let answer = sum $ map calculateCheckSum (zip [0..] shuffledBlock)
    print answer
