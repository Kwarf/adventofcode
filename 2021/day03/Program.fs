open System
open System.IO

[<Struct>]
type BitStatistics = { Positive : int; Negative: int }

let getBitStatistics value offset stats =
    match value &&& (1 <<< 11 - offset) with
    | 0 -> { stats with Negative = stats.Negative + 1; }
    | _ -> { stats with Positive = stats.Positive + 1; }

let summarizeStatistics = Array.fold (fun acc x -> Array.mapi (getBitStatistics x) acc) (Array.zeroCreate 12)

let commonize op = Seq.fold (fun acc x -> (acc <<< 1) ||| (if op x.Positive x.Negative then 1 else 0)) 0

let rec filterize op (input: int array) idx =
    match input.Length with
    | 1 -> input.[0]
    | _ ->
        let stats = Array.item idx (summarizeStatistics input)
        let cond = match stats.Positive - stats.Negative with
                   | 0 -> if op 1 0 then (<>) else (=)
                   | _ -> if op stats.Positive stats.Negative then (<>) else (=)
        filterize op (Array.filter (fun x -> cond (x &&& (1 <<< 11 - idx)) 0) input) (idx + 1)

[<EntryPoint>]
let main _ =
    let input =
        File.ReadAllLines "input.txt"
        |> Array.map (fun x -> Convert.ToInt32 (x, 2))

    let day1 = summarizeStatistics input
    Console.WriteLine $"The answer to the first part is: {commonize (>) day1 * commonize (<) day1}"
    Console.WriteLine $"The answer to the second part is: {filterize (>) input 0 * filterize (<) input 0}"
    0
