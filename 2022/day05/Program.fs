﻿open System
open System.IO
open System.Text.RegularExpressions

type Instruction = { Qty: int; From: int; To: int }

let input = File.ReadAllLines("input.txt")

let containers =
    let rows =
        input
        |> Seq.takeWhile (fun s -> not (s.StartsWith(" 1")))
        |> Seq.rev
        |> Seq.toList
    Seq.toList <| Seq.map (fun i -> rows |> List.map (fun r -> r[1 + i * 4]) |> List.where ((<>) ' ')) [ 0 .. rows[0].Length / 4 ]

let instructions =
    let rx = Regex("(\\d+).*(\\d+).*(\\d+)", RegexOptions.Compiled)
    let conv = int << string
    input
    |> Seq.where (fun x -> x.StartsWith("move"))
    |> Seq.map rx.Match
    |> Seq.map (fun x -> { Qty = conv x.Groups[1]; From = conv x.Groups[2]; To = conv x.Groups[3] })
    |> Seq.toList

let rec partOne (state: char list list) instruction =
    match instruction with
    | x when x.Qty = 0 -> state
    | _ ->
        let item = List.last state[instruction.From - 1]
        partOne
            (state
             |> List.mapi (fun i x ->
                 match i with
                 | i when i + 1 = instruction.From -> x.[.. x.Length - 2]
                 | i when i + 1 = instruction.To -> List.append x [ item ]
                 | _ -> x))
            { instruction with Qty = instruction.Qty - 1 }

let partTwo state instruction =
    state
    |> List.mapi (fun i (x: 'a list) ->
        match i with
        | i when i + 1 = instruction.From -> x.[.. x.Length - instruction.Qty - 1]
        | i when i + 1 = instruction.To -> List.append x (state[instruction.From - 1] |> List.rev |> List.take instruction.Qty |> List.rev)
        | _ -> x)
    
let rec solve fn state instructions =
    match instructions with
    | head :: tail -> solve fn (fn state head) tail
    | [] -> state

printfn
   "The answer to the first part is: %s"
   (solve partOne containers instructions |> List.map List.last |> String.Concat)

printfn
   "The answer to the second part is: %s"
   (solve partTwo containers instructions |> List.map List.last |> String.Concat)
