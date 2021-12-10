import Data.Char (isSpace)
import Data.Foldable (find)
import Data.List (nub, sortBy, (\\))
import Data.Maybe (catMaybes, isNothing)
import Data.Vector (Vector, fromList, (!))

data Input = Input
  { dx :: Int,
    dy :: Int,
    numbers :: Vector Int
  }

-- Returns the height at X/Y in the input
height :: Input -> (Int, Int) -> Int
height input (x, y) = numbers input ! (x + dx input * y)

-- Returns the neigboring points, preventing out-of-bounds access
neigbors :: Input -> (Int, Int) -> [(Int, Int)]
neigbors input (x, y) = catMaybes [left, right, above, below]
  where
    left = if x > 0 then Just (x - 1, y) else Nothing
    right = if x + 1 < dx input then Just (x + 1, y) else Nothing
    above = if y > 0 then Just (x, y - 1) else Nothing
    below = if y + 1 < dy input then Just (x, y + 1) else Nothing

-- Returns true if the point at X/Y is a low point among its neighbors
lowpoint :: Input -> (Int, Int) -> Bool
lowpoint input p = isNothing . find (<= lheight) $ nheights
  where
    lheight = height input p
    nheights = map (height input) (neigbors input p)

-- Returns the coordinates of all lowpoints in input
lowpoints :: Input -> [(Int, Int)] -> Int -> [(Int, Int)]
lowpoints input acc i
  | i < length (numbers input) = lowpoints input next (i + 1)
  | otherwise = acc
  where
    p = (i `mod` dx input, i `div` dx input)
    next = if lowpoint input p then p : acc else acc

-- Does a flood fill to determine all members in a "basin"
basinSize :: Input -> [(Int, Int)] -> [(Int, Int)] -> Int
basinSize _ from [] = (length . nub) from
basinSize input from to = basinSize input (from ++ to) nps
  where
    cond x = height input x < 9
    nps = nub $ filter cond $ concatMap (neigbors input) to \\ from

parse :: [Char] -> Input
parse content = Input dx dy indata
  where
    lns = lines content
    dx = length . head $ lns
    dy = length lns
    indata = fromList . map (read . pure :: Char -> Int) $ filter (not . isSpace) content

partOne :: Input -> [(Int, Int)] -> Int
partOne input = sum . map risk
  where
    risk p = 1 + height input p

partTwo :: Input -> [(Int, Int)] -> Int
partTwo input points = product $ take 3 soredSizes
  where
    size x = basinSize input [] [x]
    soredSizes = sortBy (flip compare) $ map size points

main = do
  indata <- parse <$> readFile "input.txt"
  let lpts = lowpoints indata [] 0

  putStrLn $
    "The answer to the first part is: "
      ++ show (partOne indata lpts)

  putStrLn $
    "The answer to the second part is: "
      ++ show (partTwo indata lpts)
