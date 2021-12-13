import Data.Bifunctor (Bifunctor (first, second))
import Data.Char (isNumber)
import Data.List (nub)
import Data.List.Split (splitOn)

type Dot = (Int, Int)

type Fold = (Int, Int)

readInt :: String -> Int
readInt = read :: String -> Int

bounds :: [Dot] -> (Int, Int)
bounds = foldr maxTuple (0, 0)

parseCoordinates :: String -> [Dot]
parseCoordinates input = map toTuple $ filter isCoordinate $ lines input
  where
    isCoordinate s = (not . null) s && (isNumber . head) s
    toTuple x =
      let pair = splitOn "," x
       in (readInt (head pair), readInt (last pair))

parseFolds :: String -> [Fold]
parseFolds input = map toTuple $ filter isFold $ lines input
  where
    isFold s = (not . null) s && ((== 'f') . head) s
    parseFold s
      | last (head s) == 'y' = (0, readInt (last s))
      | otherwise = (readInt (last s), 0)
    toTuple = parseFold . splitOn "="

applyFold :: Fold -> [Dot] -> [Dot]
applyFold (0, y) dots = (nub . map maybeFold) dots
  where
    maybeFold d
      | snd d > y = Data.Bifunctor.second ((snd . bounds) dots -) d
      | otherwise = d
applyFold (x, 0) dots = (nub . map maybeFold) dots
  where
    maybeFold d
      | fst d > x = Data.Bifunctor.first ((fst . bounds) dots -) d
      | otherwise = d
applyFold _ _ = error "One of the fold coordinates should be zero"

maxTuple :: (Ord a) => (a, a) -> (a, a) -> (a, a)
maxTuple (a1, b1) (a2, b2) = (max a1 a2, max b1 b2)

sortTuple :: (Ord a) => (a, a) -> (a, a) -> Ordering
sortTuple (a1, b1) (a2, b2)
  | b1 < b2 = LT
  | b1 > b2 = GT
  | otherwise = compare a1 a2

render :: [Dot] -> String
render dots = putDots (0, 0) dots
  where
    dx = (fst . bounds) dots
    dy = (snd . bounds) dots
    next (x, y)
      | x >= dx = (0, y + 1)
      | otherwise = (x + 1, y)
    putDots (x, y) []
      | x < dx || y < dy = '.' : putDots (next (x, y)) []
      | otherwise = "."
    putDots (x, y) (d : ds)
      | x >= dx && x == fst d && y == snd d = '#' : '\n' : putDots (next (x, y)) ds
      | x == fst d && y == snd d = '#' : putDots (next (x, y)) ds
      | x >= dx = '.' : '\n' : putDots (next (x, y)) (d : ds)
      | otherwise = '.' : putDots (next (x, y)) (d : ds)

main = do
  coordinates <- parseCoordinates <$> readFile "input.txt"
  folds <- parseFolds <$> readFile "input.txt"

  putStrLn $
    "The answer to the first part is: "
      ++ show (length $ applyFold (head folds) coordinates)
