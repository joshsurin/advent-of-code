import Control.Monad (replicateM)

readInt :: String -> Int
readInt = read

concatNumbers :: Int -> Int -> Int
concatNumbers x y = readInt (show x ++ show y)

isValidTest :: String -> Int
isValidTest s = let testResult = readInt $ takeWhile (/=':') s
                    outcomes = possibleOutcomes $ map readInt $ words $ dropWhile (/=' ') s
                in if testResult `elem` outcomes then testResult else 0

possibleOutcomes :: [Int] -> [Int]
possibleOutcomes xs = let operatorCombos = replicateM (length xs - 1) ['+', '*', '|']
                      in map (calculateOutcome xs) operatorCombos

calculateOutcome :: [Int] -> [Char] -> Int
calculateOutcome (x:xs) ops = foldl go x (zip xs (ops ++ repeat (last ops)))
  where
    go acc (num, op) =
      case op of
        '+' -> acc + num
        '*' -> acc * num
        '|' -> concatNumbers acc num
        _ -> error "invalid operator"

main :: IO()
main = do
  contents <- lines <$> readFile "input.txt"
  let answer = sum (map isValidTest contents)
  print answer
