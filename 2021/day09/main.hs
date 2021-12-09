import Data.Char (isSpace)
import Data.Foldable (find)
import Data.Maybe (catMaybes, isNothing)
import Data.Vector (Vector, fromList, (!))

data Input = Input
  { dx :: Int,
    dy :: Int,
    numbers :: Vector Int
  }

-- Returns the height at X/Y in the input
height :: Input -> Int -> Int -> Int
height input x y = numbers input ! (x + dx input * y)

-- Returns true if the point at X/Y is a low point among its neighbors
lowpoint :: Input -> Int -> Int -> Bool
lowpoint input x y = isNothing . find (<= lheight) $ catMaybes [left, right, above, below]
  where
    lheight = height input x y
    left = if x > 0 then Just $ height input (x - 1) y else Nothing
    right = if x + 1 < dx input then Just $ height input (x + 1) y else Nothing
    above = if y > 0 then Just $ height input x (y - 1) else Nothing
    below = if y + 1 < dy input then Just $ height input x (y + 1) else Nothing

parse :: [Char] -> Input
parse content = Input dx dy indata
  where
    lns = lines content
    dx = length . head $ lns
    dy = length lns
    indata = fromList . map (read . pure :: Char -> Int) $ filter (not . isSpace) content

partOne :: Input -> Int -> Int -> Int
partOne input acc i
  | i < length (numbers input) = partOne input (acc + risk) (i + 1)
  | otherwise = acc
  where
    x = i `mod` dx input
    y = i `div` dx input
    h = height input x y
    risk = if lowpoint input x y then 1 + h else 0

main = do
  indata <- parse <$> readFile "input.txt"

  putStrLn $
    "The answer to the first part is: "
      ++ show (partOne indata 0 0)
