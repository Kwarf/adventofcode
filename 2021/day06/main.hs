{-# LANGUAGE TupleSections #-}
import Data.List.Split (endBy)
import Data.Map (assocs, fromListWith, elems)

tick (0, a) = [(6, a), (8, a)]
tick (k, a) = [(k - 1, a)]

simulate 0 xs = xs
simulate n xs = simulate (n - 1) $ fromListWith (+) $ concatMap tick $ assocs xs

main = do
  seed <- fromListWith (+) . map ((, 1) . (read :: [Char] -> Int)) . endBy "," <$> readFile "input.txt"

  putStr "The answer to the first part is: "
  print $ sum . elems $ simulate 80 seed

  putStr "The answer to the second part is: "
  print $ sum . elems $ simulate 256 seed
