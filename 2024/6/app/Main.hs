import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

position :: Eq a => a -> [a] -> Int
position x xs = case elemIndex x xs of
    Just index -> index
    Nothing    -> -1

getColumnIndex :: Char -> String -> Int
getColumnIndex x xs = position x xs

getGuardPosition :: [String] -> (Int, Int)
getGuardPosition xs = findGuardPosition 0 xs
  where
    findGuardPosition _ [] = (-1, -1)
    findGuardPosition rowIndex (row:rows) =
        let columnIndex = getColumnIndex '^' row
        in if columnIndex /= -1
           then (rowIndex, columnIndex)
           else findGuardPosition (rowIndex + 1) rows

main :: IO ()
main = do
  contents <- lines <$> readFile "testInput.txt"
  let startingGuardPosition = getGuardPosition contents
  print startingGuardPosition
