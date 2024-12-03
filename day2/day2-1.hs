isIncreasing :: [Int] -> Int
isIncreasing [_] = 1
isIncreasing (a : b : sx)
  | a < b && (b - a) < 4 = isIncreasing (b : sx)
  | otherwise = 0

isDecreasing :: [Int] -> Int
isDecreasing [_] = 1
isDecreasing (a : b : sx)
  | a > b && (a - b) < 4 = isDecreasing (b : sx)
  | otherwise = 0

-- Start with first two elements to figure out direction
isGradual :: [Int] -> Int
isGradual (a : b : sx)
  | a > b = isDecreasing (a : b : sx)
  | a < b = isIncreasing (a : b : sx)
  | otherwise = 0

main = do
  values <- readFile "data.txt"

  print (sum (map (isGradual . map (\x -> read x :: Int)) (words <$> lines values)))