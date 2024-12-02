import Data.IntMap.Lazy (IntMap, alter, empty, findWithDefault)
import Data.List (sort)
import Data.Maybe (fromJust, isJust)

batch :: ([Int], [Int]) -> [Int] -> ([Int], [Int])
batch l [] = l
batch (left, right) (a : b : sx) = batch (left ++ [a], right ++ [b]) sx

main = do
  values <- readFile "data.txt"
  let (l, r) = batch ([], []) (map read $ words values :: [Int])
  let dict = foldr (alter (\x -> if isJust x then Just (1 + fromJust x) else Just 1)) (empty :: IntMap Int) r

  print (sum (map (\v -> v * findWithDefault 0 v dict) l))