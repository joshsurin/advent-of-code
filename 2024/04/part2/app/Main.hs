import Data.Maybe (fromMaybe)

countCrossMAS :: Int -> Int -> [String] -> Int
countCrossMAS row column contents =
  let charAt r c = fromMaybe ' ' (safeIndex r contents >>= safeIndex c)
      positions = [(row, column), (row + 1, column + 1), (row + 1, column - 1), (row - 1, column + 1), (row - 1, column - 1)]
      checkSequence pos = map (\(r, c) -> charAt r c) pos == "ASSMM" || map (\(r, c) -> charAt r c) pos == "ASMSM" || map (\(r, c) -> charAt r c) pos == "AMMSS" || map (\(r, c) -> charAt r c) pos == "AMSMS"
      countXMAS = if checkSequence positions then 1 else 0
  in countXMAS

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs
  | i >= 0 && i < length xs = Just (xs !! i)
  | otherwise = Nothing

main :: IO ()
main = do
  contents <- lines <$> readFile "input.txt"
  let answer = foldl (\acc (row, word) -> acc + foldl (\acc' (column, _) -> acc' + countCrossMAS row column contents) 0 (zip [0..] word)) 0 (zip [0..] contents)
  print answer
