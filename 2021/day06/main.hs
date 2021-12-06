import Data.List.Split (endBy)

tick 0 = [6, 8]
tick x = [x - 1]

simulate 0 xs = xs
simulate n xs = simulate (n - 1) xs >>= tick

main = do
  seed <- map (read :: [Char] -> Int) . endBy "," <$> readFile "input.txt"

  putStr "The answer to the first part is: "
  print $ length $ simulate 80 seed
