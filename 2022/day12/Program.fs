open System.Collections.Generic

let input = System.IO.File.ReadAllLines "input.txt"

let grid =
    let elevation c =
        match c with
        | 'S' -> int 'a'
        | 'E' -> int 'z'
        | _ -> int c
    input |> Array.map (Array.map elevation << Seq.toArray) |> Array.collect id

let neigbors idx =
    let gw = input[0].Length
    let x, y = idx % gw, idx / gw
    [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]
    |> Seq.filter (fun (x, y) -> x >= 0 && y >= 0 && x < gw && y < grid.Length / gw)
    |> Seq.map (fun (x, y) -> x + gw * y)

let distance cond goal =
    let visited = new HashSet<int>()
    let rec loop paths =
        match List.tryFind (cond << List.head) paths with
        | Some(path) -> path.Length - 1
        | _ -> loop (paths
            |> List.map (fun path ->
                let pos = List.head path
                let isAccessible a = Array.get grid a + 1 >= Array.get grid pos
                neigbors pos
                |> Seq.filter isAccessible
                |> Seq.filter (not << visited.Contains)
                |> Seq.map (fun x ->
                    visited.Add(x) |> ignore
                    (x :: path)))
            |> Seq.collect id
            |> Seq.toList)
    loop [[goal]]

let locate c = input |> String.concat "" |> Seq.findIndex ((=) c)

printfn
    "The answer to the first part is: %i"
    (distance ((=) (locate 'S')) (locate 'E'))

printfn
    "The answer to the second part is: %i"
    (distance ((=) (int 'a') << (Array.get grid)) (locate 'E'))
