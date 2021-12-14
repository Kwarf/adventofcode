{-# LANGUAGE TupleSections #-}

import Control.Arrow (first)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map (Map, alter, empty, foldrWithKey, fromList, fromListWith, toList, unionWith)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

parseRules :: [String] -> [(String, Char)]
parseRules = map pair
  where
    split = splitOn " -> "
    pair s = ((head . split) s, (head . last . split) s)

-- Returns substrings by pairs, i.e. "NNCB" -> ["NN", "NC", "CB", "B"]
byTwo :: String -> [String]
byTwo (a : b : xs) = [a, b] : byTwo (b : xs)
byTwo [] = []
byTwo x = [x]

-- Used to create the initial map from the template
seed :: String -> Map String Int
seed string = foldr (alter increment) empty (byTwo string)
  where
    increment (Just x) = Just (x + 1)
    increment Nothing = Just 1

-- Takes a pair and return the two resulting pairs
resolve :: [(String, Char)] -> String -> [String]
resolve _ [letter] = [[letter]]
resolve rules pair = [[head pair, letter], [letter, last pair]]
  where
    letter = fromJust $ lookup pair rules

steps :: Int -> [(String, Char)] -> Map String Int -> Int
steps n rules m = result $ foldr (const step) m [1..n]
  where
    step = foldrWithKey fn empty
    fn p n m = unionWith (+) m ((fromList . map (,n)) $ resolve rules p)
    result x = last (occurances x) - head (occurances x)
    occurances = sort . map snd . toList . fromListWith (+) . map (first head) . toList

main = do
  content <- lines <$> readFile "input.txt"

  let template = seed . head $ content
  let rules = (parseRules . tail . tail) content

  putStrLn $
    "The answer to the first part is: "
      ++ show (steps 10 rules template)

  putStrLn $
    "The answer to the second part is: "
      ++ show (steps 40 rules template)
