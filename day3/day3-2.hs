import Data.List (elemIndex, foldl')
import Data.Text (findIndex, isPrefixOf, pack)
import Data.Text.Internal.Search
import Debug.Trace (trace)

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

-- String to match -> accumulator -> temp acc -> is active -> result
getRanges :: String -> [String] -> String -> Bool -> [String]
getRanges "" a t _ = a ++ [t]
getRanges s a t False
  | pack "do()" `isPrefixOf` pack s = getRanges (drop 4 s) a [] True
  | otherwise = getRanges (drop 1 s) a [] False
getRanges s a t True
  | pack "don't()" `isPrefixOf` pack s = getRanges (drop 7 s) (a ++ [t]) [] False
  | otherwise = getRanges (drop 1 s) a (t ++ take 1 s) True

main = do
  values <- readFile "data.txt"

  print (sum (map (\t -> foldl' foldPairs 0 (map (toPairs []) (toSets (reverse (indices (pack pattern) (pack t))) [] t))) (getRanges values [] [] True)))
