{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char (isUpper, isLower)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.List (group, sort)

type Connection = (String, String)

pair :: [a] -> (a, a)
pair [x, y] = (x, y)

parse :: String -> [Connection]
parse s = map (pair . splitOn "-") (lines s)

isBig :: String -> Bool
isBig = all isUpper

isSmall :: String -> Bool
isSmall = all isLower

type Cond = [String] -> Connection -> Maybe [String]

partOne :: Cond
partOne vs c
  | last == b && (isBig a || a `notElem` vs) = Just (a : vs)
  | last == a && (isBig b || b `notElem` vs) = Just (b : vs)
  | otherwise = Nothing
  where
    last = head vs
    a = fst c
    b = snd c

partTwo :: Cond
partTwo vs c
  | last == b && (isBig a || (a `notElem` vs || (not hasDoubleSmall && notEndpoint a))) = Just (a : vs)
  | last == a && (isBig b || (b `notElem` vs || (not hasDoubleSmall && notEndpoint b))) = Just (b : vs)
  | otherwise = Nothing
  where
    hasDoubleSmall = (any ((> 1) . length) . group . sort . filter isSmall) vs
    notEndpoint x = not (x == "start" || x == "end")
    last = head vs
    a = fst c
    b = snd c

path :: [Connection] -> Cond -> [String] -> [String]
path cs f vs
  | last == "end" = vs
  | otherwise = path cs f (head nexts)
  where
    last = head vs
    nexts = mapMaybe (f vs) cs

paths :: [Connection] -> Cond -> [[String]] -> [[String]]
paths cs f vss = complete ++ concatMap (paths cs f) nexts
  where
    complete = filter (\x -> head x == "end") vss
    incomplete = filter (\x -> head x /= "end") vss
    nexts = map (\x -> mapMaybe (f x) cs) incomplete

main = do
  indata <- parse <$> readFile "input.txt"

  putStrLn $
    "The answer to the first part is: "
      ++ show (length $ paths indata partOne [["start"]])

  putStrLn $
    "The answer to the second part is: "
      ++ show (length $ paths indata partTwo [["start"]])
