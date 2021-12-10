import Data.Maybe (fromJust, mapMaybe, catMaybes)
import Data.Tuple (swap)

-- Returns the "score" of a closing Char
score :: Char -> Int
score = fromJust . Prelude.flip lookup [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

-- True if Char is an opening bracket
isOpen :: Char -> Bool
isOpen = Prelude.flip elem "([{<"

-- Flips a character
flip :: Char -> Char
flip = fromJust . Prelude.flip lookup (ltr ++ rtl)
  where
    ltr = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
    rtl = map swap ltr

-- Iterates through a string and returns either the unexpected Char, or Nothing
findError :: [Char] -> [Char] -> Maybe Char
findError _ [] = Nothing
findError prev (x:xs)
  | isOpen x = findError (x:prev) xs
  | otherwise = if correctlyClosed then findError (tail prev) xs else Just x
  where
    correctlyClosed = head prev == Main.flip x

partOne :: [String] -> Int
partOne = sum . map score . mapMaybe (findError [])

main = do
  indata <- lines <$> readFile "input.txt"

  putStrLn $
    "The answer to the first part is: "
      ++ show (partOne indata)

