open System.IO

let range (s: string) =
    let parts = s.Split '-' |> Array.map int
    (parts[0], parts[1])

let parseRow (s: string) =
    let elves = s.Split ',' |> Array.map range
    (elves[0], elves[1])

let isOverlapping (r1, r2) =
    (fst r1 >= fst r2 && snd r1 <= snd r2) ||
    (fst r2 >= fst r1 && snd r2 <= snd r1)

printfn
    "The answer to the first part is: %i"
    (File.ReadAllLines("input.txt")
     |> Array.where (isOverlapping << parseRow)
     |> Array.length)
