{-# LANGUAGE TupleSections #-}
import Data.List.Split (endBy)
import Data.Map (elems, fromListWith, mapWithKey, keys)

costs f t = mapWithKey (\p n -> f (abs (t - p)) * n)

cheapest f cs = minimum $ map (\x -> sum $ elems $ costs f x cs) [0..maximum $ keys cs]

main = do
  indata <- fromListWith (+) . map ((,1) . (read :: [Char] -> Int)) . endBy "," <$> readFile "input.txt"

  putStrLn $ "The answer to the first part is: " ++ show (cheapest id indata)
  putStrLn $ "The answer to the second part is: " ++ show (cheapest (\x -> sum $ take x [1..]) indata)
