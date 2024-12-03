import Data.List (elemIndex, foldl')
import Data.Text (findIndex, pack)
import Data.Text.Internal.Search

pattern = "mul("

toSets :: [Int] -> [String] -> String -> [String]
toSets [] a _ = a
toSets (i : ix) a s = toSets ix (a ++ [drop (i + length pattern) s]) (take i s)

getNumericString :: String -> String
getNumericString = takeWhile (`elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'])

toPairs :: [String] -> String -> Maybe [String]
toPairs [] s = toPairs [getNumericString s] (drop (length (getNumericString s) + 1) s)
toPairs [a] s
  | (s !! length (getNumericString s)) == ')' = Just [a, getNumericString s]
  | otherwise = Nothing

foldPairs :: Int -> Maybe [String] -> Int
foldPairs a p = case p of
  Nothing -> a
  Just (l : r : _) -> a + ((read l :: Int) * (read r :: Int))

main = do
  values <- readFile "data.txt"

  print (foldl' foldPairs 0 (map (toPairs []) (toSets (reverse (indices (pack pattern) (pack values))) [] values)))
