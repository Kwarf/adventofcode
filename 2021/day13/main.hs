import Data.Bifunctor (first, second)
import Data.Char (isNumber)
import Data.List (nub, sortBy)
import Data.List.Split (splitOn)

data Paper = Paper
  { dimensions :: (Int, Int),
    dots :: [Dot]
  }

type Dot = (Int, Int)

type Fold = (Int, Int)

bounds :: [Dot] -> (Int, Int)
bounds = foldr (\(a1, b1) (a2, b2) -> (max a1 a2, max b1 b2)) (0, 0)

parseCoordinates :: [String] -> [Dot]
parseCoordinates = map toTuple . filter isCoordinate
  where
    isCoordinate s = (not . null) s && (isNumber . head) s
    toTuple x =
      let pair = splitOn "," x
       in (read . head $ pair, read . last $ pair)

parseFolds :: [String] -> [Fold]
parseFolds = map toTuple . filter isFold
  where
    isFold s = (not . null) s && ((== 'f') . head) s
    parseFold s
      | (last . head) s == 'y' = (0, read . last $ s)
      | otherwise = (read . last $ s, 0)
    toTuple = parseFold . splitOn "="

applyFold :: Fold -> Paper -> Paper
applyFold (0, y) paper = Paper (dx, y - 1) $ (nub . map maybeFold) (dots paper)
  where
    dx = (fst . dimensions) paper
    dy = (snd . dimensions) paper
    maybeFold d
      | snd d > y = second (dy -) d
      | otherwise = d
applyFold (x, 0) paper = Paper (x - 1, dy) $ (nub . map maybeFold) (dots paper)
  where
    dx = (fst . dimensions) paper
    dy = (snd . dimensions) paper
    maybeFold d
      | fst d > x = first (dx -) d
      | otherwise = d
applyFold _ _ = error "One of the fold coordinates should be zero"

sortTuple :: (Ord a) => (a, a) -> (a, a) -> Ordering
sortTuple (a1, b1) (a2, b2)
  | b1 < b2 = LT
  | b1 > b2 = GT
  | otherwise = compare a1 a2

-- render assumes dots are sorted
render :: [Dot] -> String
render dots = putDots (0, 0) dots
  where
    dx = (fst . bounds) dots
    next (x, y)
      | x >= dx = (0, y + 1)
      | otherwise = (x + 1, y)
    maybeNewline x c
      | x >= dx = [c, '\n']
      | otherwise = [c]
    putDots (x, y) [] = ""
    putDots (x, y) (d : ds)
      | x == fst d && y == snd d = maybeNewline x '#' ++ putDots (next (x, y)) ds
      | otherwise = maybeNewline x '.' ++ putDots (next (x, y)) (d : ds)

partTwo :: Paper -> [Fold] -> [Dot]
partTwo paper = dots . foldl (flip applyFold) paper

main = do
  content <- lines <$> readFile "input.txt"
  let coordinates = parseCoordinates content
  let folds = parseFolds content
  let paper = Paper (bounds coordinates) coordinates

  putStrLn $
    "The answer to the first part is: "
      ++ show (length . dots $ applyFold (head folds) paper)

  putStrLn "The answer to the second part is:"
  putStrLn (render . sortBy sortTuple $ partTwo paper folds)
