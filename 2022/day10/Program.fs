let (|Op|_|) (c: string) (s: string) = if s.StartsWith(c) then Some(s.Substring(c.Length)) else None

type Instruction =
    | Noop
    | Addx of int * int

type CPU = int * Instruction list

let parse line =
    match line with
    | Op "addx" n -> Addx (1, int n)
    | _ -> Noop

let program = System.IO.File.ReadAllLines("input.txt") |> Array.map parse |> List.ofArray

let cycle cpu =
    match List.head (snd cpu) with
    | Addx (0, num) -> CPU(fst cpu + num, List.tail (snd cpu))
    | Addx (clk, num) -> CPU(fst cpu, Addx(clk - 1, num) :: List.tail (snd cpu))
    | Noop -> CPU(fst cpu, List.tail (snd cpu))

printfn
    "The answer to the first part is: %i"
    ({ 1 .. 220 }
    |> Seq.fold
        (fun (acc, x) n -> ((if n = 20 || n >= 60 && (n - 20) % 40 = 0 then acc + (n * fst x) else acc), cycle x))
        (0, (CPU(1, program)))
    |> fst)

printfn
    "The answer to the second part is:\n%s"
    ({ 1 .. (6 * 40) }
     |> Seq.fold (fun (acc, x) _ -> (fst x :: acc, cycle x)) ([], (CPU(1, program)))
     |> fst
     |> List.rev
     |> List.mapi (fun i reg -> if abs (i % 40 - reg) < 2 then '#' else '.')
     |> Seq.chunkBySize 40
     |> Seq.map System.String
     |> String.concat "\n")
