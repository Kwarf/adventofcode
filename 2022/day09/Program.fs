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

type Position = int * int
type Rope = Position * Position

let dist a b = abs (fst b - fst a) + abs (snd b - snd a)
let (+) a b = fst a + fst b, snd a + snd b

let step (visited, rope) motion =
    let head = fst rope + motion
    let isDiagonal tail = fst head <> fst tail && snd head <> snd tail
    let dstep tail = sign (fst head - fst tail), sign (snd head - snd tail)
    let tail =
        match snd rope with
        | x when (dist head x = 2 && isDiagonal x) || dist head x < 2 -> x
        | x when isDiagonal x -> x + dstep x
        | x -> x + motion
    (Set.add tail visited, Rope(head, tail))

printfn
    "The answer to the first part is: %i"
    (motions
     |> Array.fold step (Set.empty, (Rope((0, 0), (0, 0))))
     |> fst
     |> Set.count)