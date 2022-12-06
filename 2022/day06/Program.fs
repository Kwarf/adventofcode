open System.IO

let rec findMarker i (s: string) =
    if s[i - 4 .. i - 1] |> Seq.distinct |> Seq.length = 4 then i
    else findMarker (i + 1) s

printfn
    "The answer to the first part is: %i"
    (findMarker 4 (File.ReadAllLines("input.txt")[0]))
