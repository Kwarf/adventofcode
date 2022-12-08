open System.IO

let grid =
    let number c = int c - int '0'
    File.ReadAllLines("input.txt")
    |> Array.map (Seq.map number << Seq.toArray)
    |> array2D

let width = (+) -1 << Array2D.length2
let height = (+) -1 << Array2D.length1

let at = Array2D.get grid
let from x y =
    [ { x + 1 .. width grid } |> Seq.map (fun x -> at y x)
      { y + 1 .. height grid } |> Seq.map (fun y -> at y x)
      { x - 1 .. -1 .. 0 } |> Seq.map (fun x -> at y x)
      { y - 1 .. -1 .. 0 } |> Seq.map (fun y -> at y x) ]

let visible (x, y) =
    let isLower = (>) (at y x)
    match x, y with
    | _, 0
    | 0, _ -> true
    | x, y when x = width grid || y = height grid -> true
    | _ -> from x y |> Seq.fold (fun acc x -> acc || x |> Seq.forall isLower) false

let score (x, y) =
    let rec range n ts =
        match ts with
        | ts when Seq.isEmpty ts -> n
        | ts when Seq.head ts >= (at x y) -> n + 1
        | _ -> range (n + 1) (Seq.skip 1 ts)
    from x y |> Seq.fold (fun acc x -> acc * range 0 x) 1

let indices = seq { for x in 0 .. width grid do
                    for y in 0 .. height grid do yield x, y }

printfn
    "The answer to the first part is: %i"
    (indices |> Seq.where visible |> Seq.length)

printfn
    "The answer to the second part is: %i"
    (indices |> Seq.map score |> Seq.max)
