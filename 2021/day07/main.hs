{-# LANGUAGE TupleSections #-}
import Data.List.Split (endBy)
import Data.Map (elems, fromListWith, mapWithKey, keys)

costs t = mapWithKey cost
  where
    cost p n = abs (t - p) * n

cheapest cs = minimum $ map (\x -> sum $ elems $ costs x cs) $ keys cs

main = do
  initial <- fromListWith (+) . map ((,1) . (read :: [Char] -> Int)) . endBy "," <$> readFile "input.txt"

  putStrLn $ "The answer to the first part is: " ++ show (cheapest initial)
