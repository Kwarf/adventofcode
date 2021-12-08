import Data.Functor
import Data.List.Split (endBy)
import Data.Map (fromList)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data Segment = A | B | C | D | E | F | G

type Pattern = [Segment]

isUnique :: Pattern -> Bool
isUnique p = length p `elem` [2, 3, 4, 7]

toSegment :: Char -> Segment
toSegment char = fromJust (Map.lookup char definitions)
  where
    definitions = fromList [('a', A), ('b', B), ('c', C), ('d', D), ('e', E), ('f', F), ('g', G)]

toPattern :: [Char] -> Pattern
toPattern = map toSegment

parseLine :: [Char] -> ([Pattern], [Pattern])
parseLine line = (map toPattern segments, map toPattern numbers)
  where
    parts = endBy " | " line
    segments = words . head $ parts
    numbers = words . last $ parts

numbersFrom :: [([a], [b])] -> [b]
numbersFrom = concatMap snd

partOne :: [Pattern] -> Int
partOne = length . filter isUnique

main = do
  indata <-
    readFile "input.txt"
      <&> lines
      <&> map parseLine

  putStrLn $ "The answer to part one is: " ++ (show . partOne . numbersFrom $ indata)
