import Data.List (elemIndex, sort)
import Data.Maybe (fromJust, mapMaybe)

-- True if Char is an opening bracket
isOpen :: Char -> Bool
isOpen = Prelude.flip elem "([{<"

-- Flips a character
flip :: Char -> Char
flip c = head $ mapMaybe (uncurry match) [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
  where
    match a b
      | c == a = Just b
      | c == b = Just a
      | otherwise = Nothing

-- Iterates through a string and returns either the unexpected Char, or Nothing
findError :: [Char] -> [Char] -> ([Char], Maybe Char)
findError prev [] = (prev, Nothing)
findError prev (x : xs)
  | isOpen x = findError (x : prev) xs
  | otherwise = if isCorrectlyClosed then popRecurse else errorResult
  where
    isCorrectlyClosed = head prev == Main.flip x
    popRecurse = findError (tail prev) xs
    errorResult = ([], Just x)

partOne :: [([Char], Maybe Char)] -> Int
partOne = sum . map score . mapMaybe snd
  where
    score = fromJust . Prelude.flip lookup [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

partTwo :: [([Char], Maybe Char)] -> Int
partTwo input = scores !! (length scores `div` 2)
  where
    takeIncomplete (c, m) = (\_ -> Just c) =<< m
    incomplete = mapMaybe takeIncomplete input
    valueOf c = 1 + fromJust (elemIndex c ")]}>")
    score acc [] = acc
    score acc (x : xs) = score (acc * 5 + valueOf x) xs
    scores = sort $ map (score 0 . map Main.flip) incomplete

main = do
  indata <- map (findError []) . lines <$> readFile "input.txt"

  putStrLn $
    "The answer to the first part is: "
      ++ show (partOne indata)

  putStrLn $
    "The answer to the second part is: "
      ++ show (partTwo indata)
