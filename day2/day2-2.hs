mapCardinalities :: [Int] -> [Int] -> Int
mapCardinalities c [] = if sum c > 0 then 1 else -1
mapCardinalities c [_] = if sum c > 0 then 1 else -1
mapCardinalities c (a : b : sx)
  | a > b = mapCardinalities (c ++ [-1]) (b : sx)
  | a < b = mapCardinalities (c ++ [1]) (b : sx)
  | otherwise = mapCardinalities c sx

isIncreasing :: [Int] -> Bool -> [Int] -> Int
isIncreasing a f [_] = 1
isIncreasing a f (l : r : sx)
  | l == r && not f = isIncreasing (a ++ [l]) True (r : sx)
  | l < r && (r - l) < 4 = isIncreasing (a ++ [l]) f (r : sx)
  | f = 0
  | otherwise = max (isIncreasing [] True (a ++ [l] ++ sx)) (isIncreasing a True (a ++ [r] ++ sx))

isDecreasing :: [Int] -> Bool -> [Int] -> Int
isDecreasing a f [_] = 1
isDecreasing a f (l : r : sx)
  | l == r && not f = isDecreasing (a ++ [l]) True (r : sx)
  | l > r && (l - r) < 4 = isDecreasing (a ++ [l]) f (r : sx)
  | f = 0
  | otherwise = max (isDecreasing [] True (a ++ [l] ++ sx)) (isDecreasing a True (a ++ [r] ++ sx))

-- Start with first two elements to figure out direction
getErrorState :: [Int] -> Int
getErrorState (a : b : sx)
  | mapCardinalities [] (a : b : sx) == 1 = isIncreasing [] False (a : b : sx)
  | mapCardinalities [] (a : b : sx) == -1 = isDecreasing [] False (a : b : sx)
  | otherwise = 0

main = do
  values <- readFile "data-test.txt"

  print (sum (map getErrorState (map (\x -> read x :: Int) . words <$> lines values)))
