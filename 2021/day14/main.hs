{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Function (on)
import Data.List (group, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

parseRules :: [String] -> [(String, String)]
parseRules input = allRules
  where
    split = splitOn " -> "
    pair s = (head $ split s, head . last $ split s)
    buildRule ([a, b], c) = ([a, b], [a, c])
    allRules = map (buildRule . pair) input

byTwo :: String -> [String]
byTwo (a : b : xs) = [a, b] : byTwo (b : xs)
byTwo [] = []
byTwo x = [x]

process :: [(String, String)] -> String -> String
process rules = concatMap processPair . byTwo
  where
    processPair (a : b : _) = fromJust $ lookup [a, b] rules
    processPair x = x

processSteps :: Int -> [(String, String)] -> String -> String
processSteps 0 _ input = input
processSteps n rules input = processSteps (n - 1) rules (process rules input)

partOne :: [(String, String)] -> String -> Int
partOne rules input = (length . last) groups - (length . head) groups
  where
    groups = sortBy (compare `on` length) $ group . sort $ processSteps 10 rules input

main = do
  content <- lines <$> readFile "input.txt"

  let template = head content
  let rules = (parseRules . tail . tail) content

  putStrLn $
    "The answer to the first part is: "
      ++ show (partOne rules template)
