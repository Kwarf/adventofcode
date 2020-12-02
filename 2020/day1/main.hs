findFirstAnswer list = head [ x * y | x <- list, y <- list, x + y == 2020]

findSecondAnswer list = head [ x * y * z | x <- list, y <- list, z <- list, x + y + z == 2020]

main = do
    input <- map (read :: String -> Int)
        . lines
        <$> readFile "input.txt"

    putStr "The answer to the first part is: "
    print
        . findFirstAnswer
        $ input

    putStr "The answer to the second part is: "
    print
        . findSecondAnswer
        $ input
