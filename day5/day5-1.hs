import Data.IntMap.Strict (IntMap, alter, empty, (!?))
import Data.List (intersect)
import Data.Maybe (fromJust, isJust)

getPair :: String -> (Int, Int)
getPair s = (read front :: Int, read (drop 1 back) :: Int) where (front, back) = break (== '|') s

groupByFirst :: [String] -> IntMap [Int] -> IntMap [Int]
groupByFirst sx g =
  foldl
    ( \g s ->
        alter
          ( \v ->
              if isJust v
                then
                  Just (fromJust v ++ [snd (getPair s)])
                else
                  Just [snd (getPair s)]
          )
          (fst (getPair s))
          g
    )
    g
    sx

split :: [Int] -> String -> [Int]
split a "" = a
split a s = split (a ++ [read first :: Int]) (drop 1 rest) where (first, rest) = break (== ',') s

isValid :: IntMap [Int] -> [Int] -> [Int] -> Bool
isValid _ _ [] = True
isValid d [] (a : b) = isValid d [a] b
isValid d p (a : b)
  | isJust v = null (fromJust v `intersect` p) && isValid d (p ++ [a]) b
  | otherwise = isValid d (p ++ [a]) b
  where
    v = d !? a

main = do
  values <- readFile "data.txt"
  let (rules, pages) = break (== "") (lines values)

  print (sum (map (\v -> v !! max 0 (length v `div` 2)) (filter (isValid (groupByFirst rules empty) []) (map (split []) (drop 1 pages)))))
