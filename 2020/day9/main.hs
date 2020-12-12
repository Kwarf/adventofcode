import           Data.List                      ( sort )

partOne list i = if target `notElem` sums then target else partOne list (i + 1)
 where
  target   = list !! (25 + i)
  preamble = take 25 $ drop i list
  sums     = [ x + y | x <- preamble, y <- preamble, x /= y ]

partTwo list target i | target == sum sublist = head sorted + last sorted
                      | sum sublist > target  = partTwo (drop 1 list) target 2
                      | otherwise             = partTwo list target (i + 1)
 where
  sublist = take i list
  sorted  = sort sublist

main = do
  input <- map (read :: String -> Int) . lines <$> readFile "input.txt"

  let invalidNo = partOne input 0
  putStr "The answer to the first part is: "
  print invalidNo

  putStr "The answer to the second part is: "
  print $ partTwo input invalidNo 2
