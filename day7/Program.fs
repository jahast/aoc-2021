open System
open System.IO
open System.Text.RegularExpressions

let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let coords =
    input.Item(0)
    |> Regex("\d+").Matches
    |> Seq.cast<Match>
    |> Seq.map (fun i -> int i.Value)
    |> Seq.toList

let median =
    coords
    |> List.sort
    |> (fun xs -> xs.Item(List.length xs / 2))

let fuel =
    coords
    |> List.map ((+) -median)
    |> List.map Math.Abs
    |> List.sum

printfn "%A" fuel

// hope this is convex
let rec gd xs x =
    let diffs x =
        xs
        |> List.map ((+) -x)
        |> List.map (fun n -> if n < 0 then -n else n)
        |> List.map (fun n -> n * (n + 1) / 2)
        |> List.sum

    let cur, plusOne, minusOne = diffs x, diffs (x + 1), diffs (x - 1)

    match (cur - plusOne < 0, cur - minusOne < 0) with
    | (true, true) -> cur
    | (false, true) -> gd xs (x + 1)
    | (_, false) -> gd xs (x - 1)

let ans2 = gd coords median

printfn "%A" ans2
