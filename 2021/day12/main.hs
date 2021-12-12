{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char (isUpper)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Connection = (String, String)

pair :: [a] -> (a, a)
pair [x, y] = (x, y)

parse :: String -> [Connection]
parse s = map (pair . splitOn "-") (lines s)

isBig :: String -> Bool
isBig = all isUpper

canUse :: [String] -> Connection -> Maybe [String]
canUse vs c
  | last == b && (isBig a || a `notElem` vs) = Just (a : vs)
  | last == a && (isBig b || b `notElem` vs) = Just (b : vs)
  | otherwise = Nothing
  where
    last = head vs
    a = fst c
    b = snd c

path :: [Connection] -> [String] -> [String]
path cs vs
  | last == "end" = vs
  | otherwise = path cs (head nexts)
  where
    last = head vs
    nexts = mapMaybe (canUse vs) cs

paths :: [Connection] -> [[String]] -> [[String]]
paths cs vss = complete ++ concatMap (paths cs) nexts
  where
    complete = filter (\x -> head x == "end") vss
    incomplete = filter (\x -> head x /= "end") vss
    nexts = map (\x -> mapMaybe (canUse x) cs) incomplete

main = do
  indata <- parse <$> readFile "input.txt"

  putStrLn $
    "The answer to the first part is: "
      ++ show (length $ paths indata [["start"]])
