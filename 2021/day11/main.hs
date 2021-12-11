import Data.Char (isSpace)
import Data.List (delete)
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)

data State = State
  { dx :: Int,
    numbers :: [Int],
    flashers :: [Int]
  }

instance Show State where
  show state = unlines $ map (concatMap show) lines
    where
      lines = chunksOf (dx state) (numbers state)

parse :: String -> State
parse content = State dx numbers []
  where
    lns = lines content
    dx = length . head $ lns
    numbers = map (read . pure :: Char -> Int) $ filter (not . isSpace) content

withIndex :: [a] -> [(Int, a)]
withIndex xs = next 0 xs
  where
    next i (x : xs) = (i, x) : next (i + 1) xs
    next _ _ = []

neighbors :: State -> Int -> [Int]
neighbors state n = map ptoi $ filter valid $ np (itop n)
  where
    dx = Main.dx state
    dy = (length . numbers) state `div` dx
    x = fst
    y = snd
    np p = delete (itop n) [(x, y) | x <- [x p - 1 .. x p + 1], y <- [y p - 1 .. y p + 1]]
    itop i = (i `mod` dx, i `div` dx)
    ptoi p = x p + dx * y p
    valid p = x p >= 0 && x p < dx && y p >= 0 && y p < dy

clearFlashes :: State -> State
clearFlashes state = State (dx state) (numbers state) []

step :: State -> State
step state = clearFlashes $ recTrigger (State (dx state) stepAllOne (flashers state))
  where
    -- First, the energy level of each octopus increases by 1
    stepAllOne = map (+ 1) (numbers state)
    -- Then, any octopus with an energy level greater than 9 flashes
    fIdx (i, n)
      | n > 9 = Just i
      | otherwise = Nothing
    hasFlashed :: State -> Int -> Bool -- True if the octopus at the specified index has already flashed
    hasFlashed s i = i `elem` flashers s
    toFlash :: State -> [Int] -- Returns the index of all octopuses that should flash
    toFlash s = filter (not . hasFlashed s) $ mapMaybe fIdx $ (withIndex . numbers) s
    newNumbers :: State -> Int -> [Int] -> [Int] -- Increases neigboring numbers, and zeroes the number at i
    newNumbers s i is = map (\x -> if fst x `elem` is then snd x + 1 else if fst x == i then 0 else snd x) (withIndex (numbers s))
    flash :: State -> Int -> State -- Flashes the octopus at the specified index
    flash s i = State (dx s) (newNumbers s i (filter (not . hasFlashed s) (neighbors s i))) (i : flashers s)
    recTrigger :: State -> State -- Recursive trigger of flashes
    recTrigger s
      | all (hasFlashed s) (toFlash s) = s
      | otherwise = recTrigger $ flash s (head $ toFlash s)

partOne :: State -> Int
partOne state = loop 0 state 100
  where
    countFlashes s = length $ filter (==0) (numbers s)
    loop acc _ 0 = acc
    loop acc s n = loop (acc + countFlashes (step s)) (step s) (n - 1)

main = do
  indata <- parse <$> readFile "input.txt"

  putStrLn $
    "The answer to the first part is: "
      ++ show (partOne indata)
