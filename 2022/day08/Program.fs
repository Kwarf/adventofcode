open System.IO

let input =
    let number c = int c - int '0'
    File.ReadAllLines("input.txt")
    |> Array.map (fun x -> x |> Seq.map number |> Seq.toArray)
    |> array2D

let visible (grid: int[,]) (x, y) =
    let get = Array2D.get grid
    let isLower = (>) (get y x)
    match x, y with
    | _, 0 | 0, _ -> true
    | x, y when x + 1 = Array2D.length2 grid || y + 1 = Array2D.length1 grid -> true
    | _ -> seq { x + 1 .. Array2D.length2 grid - 1 }
            |> Seq.map (fun x -> get y x)
            |> Seq.forall isLower
        || seq { x - 1 .. -1 .. 0 }
            |> Seq.map (fun x -> get y x)
            |> Seq.forall isLower
        || seq { y + 1 .. Array2D.length1 grid - 1 }
            |> Seq.map (fun y -> get y x)
            |> Seq.forall isLower
        || seq { y - 1 .. -1 .. 0 }
            |> Seq.map (fun y -> get y x)
            |> Seq.forall isLower

let indices (grid: int[,]) = seq { for x in 0 .. Array2D.length2 grid - 1 do
                                   for y in 0 .. Array2D.length1 grid - 1 do yield x, y }

printfn
    "The answer to the first part is: %i"
    (input |> indices |> Seq.where (visible input) |> Seq.length)
