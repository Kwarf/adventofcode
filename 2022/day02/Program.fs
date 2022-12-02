open System.IO

let score (a, b) =
    let b = char (int b - 23)
    match a, b with
    | a, b when a = b -> 3
    | 'A', 'B' | 'B', 'C' | 'C', 'A' -> 6
    | _ -> 0
    + int b - 64

printfn
    "The answer to the first part is: %i"
    (File.ReadAllLines("input.txt") |> Array.sumBy (score << fun s -> (s.Chars 0, s.Chars 2)))
