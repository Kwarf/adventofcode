open System
open System.IO

let duplicates sack =
    let len = String.length sack / 2
    Set.intersect (Set.ofSeq sack.[..len-1]) (Set.ofSeq sack.[len..])
    |> Set.toArray

let priority c = if Char.IsLower c then int c - 96 else int c - 38

printfn
    "The answer to the first part is: %i"
    (File.ReadAllLines("input.txt") |> Array.sumBy (Array.sumBy priority << duplicates))