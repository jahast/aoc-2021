open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let fish =
    input.Item(0)
    |> Regex("\d+").Matches
    |> Seq.cast<Match>
    |> Seq.map (fun i -> int i.Value)
    |> Seq.toList


let rec grow xs gens =
    let reduced = xs |> List.map ((+) -1)

    let nNewFish =
        reduced |> List.filter ((=) -1) |> List.length

    let reset =
        reduced
        |> List.map (fun x -> if x = -1 then 6 else x)

    let newGen = reset @ (List.replicate nNewFish 8)
    let newGenIndex = gens - 1

    match newGenIndex with
    | 0 -> newGen
    | _ -> grow newGen (gens - 1)

let ans1 = grow fish 80 |> List.length

printfn "%A" ans1

let memoize wrapFunction =
    let cache = Dictionary()

    let rec f x =
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ ->
            let v = wrapFunction f x
            cache.[x] <- v
            v

    f

let growOne =
    memoize
        (fun (f: int64 -> int64) gen ->
            let nine = int64 9
            let seven = int64 7
            let one = int64 1

            match gen with
            | _ when gen < nine -> 1
            | _ ->
                one
                + f (gen - nine)
                + ([ (gen - nine - seven) .. -seven .. 0 ]
                   |> List.map f
                   |> List.sum))

let gens = 256

let ans2 =
    fish
    |> List.map (fun x -> gens + (8 - x))
    |> List.map (int64 >> growOne)
    |> List.sum

printfn "%A" ans2
