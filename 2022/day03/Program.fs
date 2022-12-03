open System
open System.IO

let duplicates sack =
    let len = String.length sack / 2
    Set.intersect (Set.ofSeq sack.[..len-1]) (Set.ofSeq sack.[len..])
    |> Set.toArray

let priority c = if Char.IsLower c then int c - 96 else int c - 38

let input = File.ReadAllLines("input.txt")

printfn
    "The answer to the first part is: %i" 
    (input |> Array.sumBy (Array.sumBy priority << duplicates))

printfn
    "The answer to the second part is: %i"
    (input
     |> Array.chunkBySize 3
     |> Array.map (fun x -> Set.intersectMany (x |> Array.map Set.ofSeq))
     |> Array.sumBy (priority << Seq.head << Set.toSeq))
