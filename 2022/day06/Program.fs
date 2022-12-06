open System.IO

let rec findMarker n i (s: string) =
    if s[i - n .. i - 1] |> Seq.distinct |> Seq.length = n then i
    else findMarker n (i + 1) s

let input = File.ReadAllLines("input.txt")[0]

printfn
    "The answer to the first part is: %i"
    (findMarker 4 4 input)

printfn
    "The answer to the second part is: %i"
    (findMarker 14 14 input)
