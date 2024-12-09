import Data.Array
import Debug.Trace (trace)
import Debug.Trace qualified as Debug

type Matrix = Array Int (Array Int Char)

type Point = (Int, Int)

data HorizontalDirection = Left | Center | Right

data VerticalDirection = Current | Next | Previous

toHorizontalDirection :: HorizontalDirection -> Int
toHorizontalDirection h = case h of
  Main.Left -> -1
  Main.Center -> 0
  Main.Right -> 1

toVerticalDirection :: VerticalDirection -> Int
toVerticalDirection v = case v of
  Previous -> -1
  Current -> 0
  Next -> 1

getColumnsLength :: Matrix -> Int
getColumnsLength m = length (m ! (length m - 1)) - 1

walkMatrix :: Matrix -> Point -> Point
walkMatrix m (i, j) = (if j == getColumnsLength m then i + 1 else i, if j == getColumnsLength m then 0 else j + 1)

move :: Matrix -> VerticalDirection -> HorizontalDirection -> Point -> Point
move m v h (i, j) = (i + toVerticalDirection v, j + toHorizontalDirection h)

getChar :: Matrix -> Point -> Char
getChar m (i, j)
  | i > length m - 1 || i < 0 || j > getColumnsLength m || j < 0 = '-'
  | otherwise = (m ! i) ! j

isWord :: Matrix -> VerticalDirection -> HorizontalDirection -> Point -> String -> Int
isWord m v h p "XMAS" = 1
isWord m v h (i, j) s
  | length s == 4 = 0
  | otherwise = isWord m v h (move m v h (i, j)) (s ++ [Main.getChar m (i, j)])

findWord :: Matrix -> Point -> Int
findWord m (i, j)
  | (m ! i) ! j == 'X' =
      sum
        ( map
            fromEnum
            [ isWord m Main.Previous Main.Left (i, j) "",
              isWord m Main.Previous Main.Center (i, j) "",
              isWord m Main.Previous Main.Right (i, j) "",
              isWord m Main.Current Main.Left (i, j) "",
              isWord m Main.Current Main.Center (i, j) "",
              isWord m Main.Current Main.Right (i, j) "",
              isWord m Main.Next Main.Left (i, j) "",
              isWord m Main.Next Main.Center (i, j) "",
              isWord m Main.Next Main.Right (i, j) ""
            ]
        )
  | otherwise = 0

-- Given a matrix of characters and a starting point return a count of matched words
countWords :: Matrix -> Point -> Int -> Int
countWords m (i, j) c
  -- Exit if out of bounds
  | i == length m || j == getColumnsLength m + 1 = c
  | otherwise = countWords m (walkMatrix m (i, j)) c + findWord m (i, j)

main :: IO ()
main = do
  values <- readFile "data.txt"
  let valueLines = foldl (\a r -> a ++ [listArray (0, length r - 1) r]) [] (lines values)

  print (countWords (listArray (0, length valueLines - 1) valueLines) (0, 0) 0)