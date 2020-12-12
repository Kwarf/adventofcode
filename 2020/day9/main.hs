partOne list i = if target `notElem` sums then target else partOne list (i + 1)
 where
  target   = list !! (25 + i)
  preamble = take 25 $ drop i list
  sums     = [ x + y | x <- preamble, y <- preamble, x /= y ]

main = do
  input <- map (read :: String -> Int) . lines <$> readFile "input.txt"

  putStr "The answer to the first part is: "
  print $ partOne input 0
