open System.IO

let split fn (c: char) (s: string) =
    let parts = s.Split c |> Array.map fn
    (parts[0], parts[1])

let isFullyOverlapping (r1, r2) = fst r1 >= fst r2 && snd r1 <= snd r2 || fst r2 >= fst r1 && snd r2 <= snd r1

let isOverlapping (r1, r2) = not (snd r1 < fst r2 || fst r2 > snd r1 || fst r1 > snd r2 || snd r2 < fst r1)

let input = File.ReadAllLines("input.txt") |> Array.map (split (split int '-') ',')

printfn
    "The answer to the first part is: %i"
    (input |> Seq.filter isFullyOverlapping |> Seq.length)

printfn
    "The answer to the second part is: %i"
    (input |> Seq.filter isOverlapping |> Seq.length)
