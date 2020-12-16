import           Data.IntSet                    ( findMax
                                                , IntSet
                                                , fromList
                                                )
import           Data.IntMap                    ( (!)
                                                , findWithDefault
                                                , fromSet
                                                , IntMap
                                                )
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

partTwo list = map
 where
  t = findMax list
  map = fromSet
    (\x -> if x == t
      then 1
      else sum [ findWithDefault 0 (x + i) map | i <- [1 .. 3] ]
    )
    list

main = do
  input <- map (read :: String -> Int) . lines <$> readFile "input.txt"

  putStr "The answer to the first part is: "
  print . tmul $ partOne (partOneInit input) (sort input)

  putStr "The answer to the second part is: "
  print $ partTwo (fromList $ 0 : maximum input + 3 : input) ! 0
