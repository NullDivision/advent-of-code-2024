import Data.Array

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

-- Tokenize characters
charToInt :: Char -> Int
charToInt 'M' = 1
charToInt 'S' = 2
-- Extreme negative weight on any other characters to avoid them
charToInt _ = -999

-- Sum up character tokens
countCorners :: Matrix -> Point -> Int
countCorners m p =
  sum
    [ charToInt (Main.getChar m (move m Previous Main.Left p)),
      charToInt (Main.getChar m (move m Previous Main.Right p)),
      charToInt (Main.getChar m (move m Next Main.Left p)),
      charToInt (Main.getChar m (move m Next Main.Right p))
    ]

isMirrored :: Matrix -> Point -> Bool
isMirrored m p = Main.getChar m (move m Previous Main.Left p) /= Main.getChar m (move m Next Main.Right p)

-- If word is matched return 1 otherwise 0
countWord :: Matrix -> Point -> Int
countWord m (i, j)
  | (m ! i) ! j == 'A' && countCorners m (i, j) == 6 && isMirrored m (i, j) = 1
  | otherwise = 0

-- Given a matrix of characters and a starting point return a count of matched words
countWords :: Matrix -> Point -> Int -> Int
countWords m (i, j) c
  -- Exit if out of bounds
  | i == length m || j == getColumnsLength m + 1 = c
  | otherwise = countWords m (walkMatrix m (i, j)) c + countWord m (i, j)

main :: IO ()
main = do
  values <- readFile "data.txt"
  let valueLines = foldl (\a r -> a ++ [listArray (0, length r - 1) r]) [] (lines values)

  -- Start from second character to include a 3x3 segment
  print (countWords (listArray (0, length valueLines - 1) valueLines) (1, 1) 0)