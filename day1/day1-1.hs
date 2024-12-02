import Data.List (sort)

batch :: ([Int], [Int]) -> [Int] -> ([Int], [Int])
batch l [] = l
batch (left, right) (a : b : sx) = batch (left ++ [a], right ++ [b]) sx

toSorted :: ([Int], [Int]) -> ([Int], [Int])
toSorted (l, r) = (sort l, sort r)

toDistance :: [Int] -> ([Int], [Int]) -> [Int]
toDistance c ([], []) = c
toDistance c (l : lx, r : rx) = toDistance (c ++ [abs (l - r)]) (lx, rx)

main = do
  values <- readFile "data.txt"

  print (sum (toDistance [] (toSorted (batch ([], []) (map read $ words values :: [Int])))))