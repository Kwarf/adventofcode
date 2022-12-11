let (|Direction|_|) (d: char) (s: string) = if s.StartsWith(d) then Some(s.Substring(2)) else None

let parse line =
    let moves = Array.replicate << int
    match line with
    | Direction 'U' dist -> (0, -1) |> moves dist
    | Direction 'D' dist -> (0,  1) |> moves dist
    | Direction 'L' dist -> (-1, 0) |> moves dist
    | Direction 'R' dist -> ( 1, 0) |> moves dist
    | _ -> Array.empty

let motions =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map parse
    |> Array.collect id

let dist a b = abs (fst b - fst a) + abs (snd b - snd a)
let add a b = fst a + fst b, snd a + snd b

let step (visited, rope) motion =
    let isDiagonal h t = fst h <> fst t && snd h <> snd t
    let step h t = sign (fst h - fst t), sign (snd h - snd t)
    let list =
        rope
        |> List.skip 1
        |> List.fold
            (fun n x ->
                let head = List.head n
                match x with
                | x when dist head x = 2 && isDiagonal head x || dist head x < 2 -> x
                | x -> add x (step head x)
                :: n)
            [ add (List.head rope) motion ]
        |> List.rev
    (Set.add (List.last list) visited, list)

printfn
    "The answer to the first part is: %i"
    (motions
     |> Array.fold step (Set.empty, ([(0, 0); (0, 0)]))
     |> fst
     |> Set.count)

printfn
    "The answer to the second part is: %i"
    (motions
     |> Array.fold step (Set.empty, ((0, 0)) |> List.replicate 10)
     |> fst
     |> Set.count)
