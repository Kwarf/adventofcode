import           Data.List                      ( sort )

oneif b = if b then 1 else 0
tmul (a, b) = a * b

partOneInit list = (oneif $ f == 1, oneif $ f == 3) where f = minimum list

partOne (o, t) [_     ] = (o, t + 1)
partOne (o, t) (x : xs) = foldl partOne (no, nt) [xs]
 where
  next = head xs
  no   = if x + 1 == next then o + 1 else o
  nt   = if x + 3 == next then t + 1 else t

main = do
  input <- map (read :: String -> Int) . lines <$> readFile "input.txt"

  putStr "The answer to the first part is: "
  print . tmul $ partOne (partOneInit input) (sort input)
