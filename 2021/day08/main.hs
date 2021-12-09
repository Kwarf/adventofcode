import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (delete, find, intersect, sort, sortOn, (\\))
import Data.List.Split (endBy)
import Data.Maybe (fromJust)
import Data.Sequence (fromList, mapWithIndex)

type Pattern = [Char]
type Mapping = (Pattern, Int)

isUnique :: Pattern -> Bool
isUnique p = length p `elem` [2, 3, 4, 7]

parseLine :: [Char] -> ([Pattern], [Pattern])
parseLine line = (map sort segments, map sort numbers)
  where
    parts = endBy " | " line
    segments = sortOn length . words . head $ parts
    numbers = words . last $ parts

numbersFrom :: [([a], [b])] -> [b]
numbersFrom = concatMap snd

partOne :: [Pattern] -> Int
partOne = length . filter isUnique

analyze :: [Pattern] -> [Mapping]
analyze patterns = [(one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9), (zero, 0)]
  where
    -- known numbers
    [one, seven, four] = take 3 patterns
    eight = last patterns
    -- groups by number of segments (patterns are sorted by length during parse)
    fiveGroup = drop 3 patterns & take 3
    sixGroup = drop 6 patterns & take 3
    -- taking the 6-group (6/9/0) and removing the known four, looking for 2 remaining segments, must give the nine
    nine = fromJust $ find (\x -> 2 == length (x \\ four)) sixGroup
    -- the six must be the one remaining in the 6-group (6/0) that upon intersection with one only has one segment
    six = fromJust $ find (\x -> 1 == length (x `intersect` one)) $ sixGroup & delete nine 
    -- and now we know zero
    zero = head $ sixGroup \\ [nine, six]
    -- working on the 5-group (2/3/5), intersecting with seven, what then only has 2 segments must be three
    three = fromJust $ find (\x -> 2 == length (x \\ seven)) fiveGroup
    -- the same check for one segment against the six (having 2/5) will allow us to get the five
    five = fromJust $ find (\x -> 1 == length (six \\ x)) $ fiveGroup & delete three
    -- and then we only have the two left
    two = head $ fiveGroup \\ [three, five]

resolve :: ([Pattern], [Pattern]) -> Int
resolve (patterns, numbers) = sum . mapWithIndex num $ fromList numbers
  where
    lut = analyze patterns
    num i x = 10 ^ (3 - i) * fromJust (lookup x lut)

partTwo :: [([Pattern], [Pattern])] -> Int
partTwo = sum . map resolve

main = do
  indata <-
    readFile "input.txt"
      <&> lines
      <&> map parseLine

  putStrLn $
    "The answer to the first part is: "
      ++ (show . partOne . numbersFrom $ indata)

  putStrLn $
    "The answer to the second part is: "
      ++ (show . partTwo $ indata)
