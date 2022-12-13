open System

type Token = Begin | End | Value of int

let replace (a: string) (b: string) (s: string) = s.Replace(a, b)
let split (sep: string) (s: string) = s.Split(sep, StringSplitOptions.RemoveEmptyEntries)

let toToken s = match s with | "[" -> Begin | "]" -> End | _ -> Value (Int32.Parse s)
let tokenize s = s |> replace "[" "[," |> replace "]" ",]" |> split "," |> Seq.map toToken |> Seq.toList

let isInOrder (l, r) =
    let asList n = [Begin; Value n; End]
    let rec loop l r =
        match l, r with
        | l::ls      , r::rs when l = r -> loop ls rs
        | Value l::_ , Value r::_       -> l < r
        | Begin::_   , Value r::rs      -> loop l (asList r @ rs)
        | Value l::ls, Begin::_         -> loop (asList l @ ls) r
        | End::_     , _                -> true
        | _                             -> false
    loop (tokenize l) (tokenize r)

let pairs =
    IO.File.ReadAllLines "input.txt"
    |> Array.chunkBySize 3
    |> Array.map (fun x -> x[0], x[1])

printfn
    "The answer to the first part is: %i"
    (pairs |> Seq.indexed |> Seq.filter (isInOrder << snd) |> Seq.sumBy ((+) 1 << fst))

printfn
    "The answer to the second part is: %A"
    (pairs
    |> Array.append [|("[[2]]","[[6]]")|]
    |> Array.collect (fun x -> [|fst x;snd x|])
    |> Seq.sortWith (fun l r -> if isInOrder (l, r) then -1 else 1)
    |> Seq.indexed
    |> Seq.fold (fun acc (i, s) -> acc * match s with | "[[2]]" | "[[6]]" -> i + 1 | _ -> 1) 1)
