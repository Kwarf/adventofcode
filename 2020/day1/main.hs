targetValue = (2020 -)

findFirstAnswer (x:xs) = if targetValue x `elem` xs
    then x * targetValue x
    else findFirstAnswer xs

main = do
    input <- readFile "input.txt"

    putStr "The answer to the first part is: "
    print
        . findFirstAnswer
        . map (read :: String -> Int)
        . lines
        $ input
