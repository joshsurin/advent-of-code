import Data.Maybe (fromMaybe)

countSpellsXMAS :: Int -> Int -> [String] -> Int
countSpellsXMAS row column contents =
  let charAt r c = fromMaybe ' ' (safeIndex r contents >>= safeIndex c)
      positions = [[(row, column), (row + 1, column), (row + 2, column), (row + 3, column)],
                   [(row, column), (row, column + 1), (row, column + 2), (row, column + 3)],
                   [(row, column), (row + 1, column + 1), (row + 2, column + 2), (row + 3, column + 3)],
                   [(row, column), (row , column - 1), (row, column - 2), (row, column - 3)],
                   [(row, column), (row - 1, column), (row - 2, column), (row - 3, column)],
                   [(row, column), (row - 1, column - 1), (row - 2, column - 2), (row - 3, column - 3)],
                   [(row, column), (row + 1, column - 1), (row + 2, column - 2), (row + 3, column - 3)],
                   [(row, column), (row - 1, column + 1), (row - 2, column + 2), (row - 3, column + 3)] ]
      checkSequence pos = map (\(r, c) -> charAt r c) pos == "XMAS"
      countXMAS = length $ filter checkSequence positions
  in countXMAS

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs
  | i >= 0 && i < length xs = Just (xs !! i)
  | otherwise = Nothing

main :: IO ()
main = do
  contents <- lines <$> readFile "input.txt"
  let answer = foldl (\acc (row, word) -> acc + foldl (\acc' (column, _) -> acc' + countSpellsXMAS row column contents) 0 (zip [0..] word)) 0 (zip [0..] contents)
  print answer
