open System
open System.IO

[<Struct>]
type BitStatistics = { Positive : int; Negative: int }

let intFromBinary x = Convert.ToInt32 (x, 2)

let dataFrom path =
    File.ReadAllLines path
    |> Array.map intFromBinary

let getBitStatistics value offset stats =
    match value &&& (1 <<< offset) with
    | 0 -> { stats with Negative = stats.Negative + 1; }
    | _ -> { stats with Positive = stats.Positive + 1; }

let calculateStatistics state value = Array.mapi (getBitStatistics value) state

let summarize op = Seq.rev >> Seq.fold (fun acc x -> (acc <<< 1) ||| (if op x.Positive x.Negative then 1 else 0)) 0

[<EntryPoint>]
let main _ =
    let input =
        dataFrom "input.txt"
        |> Array.fold calculateStatistics (Array.zeroCreate 12)

    Console.WriteLine $"The answer to the first part is: {summarize (>) input * summarize (<) input}"
    0
