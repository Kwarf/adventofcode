open System.IO

let p1 (a, b) =
    let b = char (int b - 23)
    match a, b with
    | a, b when a = b -> 3
    | 'A', 'B' | 'B', 'C' | 'C', 'A' -> 6
    | _ -> 0
    + int b - 64

let p2 (a, b) =
    let x = match b with 'Y' -> 3 | 'Z' -> 6 | _ -> 0
    let y = match x, a with 
            | 6, 'A' -> 'B' | 6, 'B' -> 'C' | 6, 'C' -> 'A'
            | 0, 'A' -> 'C' | 0, 'B' -> 'A' | 0, 'C' -> 'B'
            | _, a -> a
    x + int y - 64

let input = File.ReadAllLines("input.txt") |> Array.map (fun s -> (s.Chars 0, s.Chars 2))

printfn
    "The answer to the first part is: %i"
    (input |> Array.sumBy p1)
printfn
    "The answer to the second part is: %i"
    (input |> Array.sumBy p2)
