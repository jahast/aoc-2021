open System
open System.Collections.Generic
open System.IO
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let start =
    input.Item(0) |> List.ofSeq |> List.map string

let split s = s |> List.ofSeq |> List.map string

let merge (map1: Map<string, int64>) (map2: Map<string, int64>) =
    Map.fold
        (fun acc key value ->
            let found =
                Option.defaultValue (int64 0) (Map.tryFind key acc)

            Map.add key (value + found) acc)
        map1
        map2

let rules =
    input
    |> List.skip 2
    |> List.map
        (fun x ->
            x.Split("->", StringSplitOptions.TrimEntries)
            |> List.ofArray)
    |> List.map (fun xs -> (xs.Item(0), xs.Item(1)))
    |> Map


let grow (pol: string list) =
    pol
    |> List.windowed 2
    |> List.map (String.concat "")
    |> List.map (fun s -> String.concat "" [ s.Substring(0, 1); Map.find s rules ])
    |> List.append
    <| [ List.last pol ]
    |> String.concat ""
    |> split


let rec growMany n (pol: string list) =
    match n with
    | 0 -> pol
    | _ -> growMany (n - 1) (grow pol)

let ans1 =
    growMany 10 start
    |> List.countBy id
    |> List.map snd
    |> List.sort
    |> (fun xs -> (List.last xs) - (List.head xs))

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

let toMap xs =
    xs
    |> List.countBy id
    |> List.map (fun x -> (fst x, (snd >> int64) x))
    |> Map

let one = int64 1

let growBetter =
    memoize
        (fun (f: (string list) * int -> Map<string, int64>) (input: (string list) * int) ->
            let round = (snd input)
            let nextRound = round - 1
            let pol = fst input
            let key = String.concat "" pol

            let insertion = Map.find key rules
            let leftNew = [ pol.Item(0); insertion ]
            let rightNew = [ insertion; pol.Item(1) ]

            match round with
            | 1 -> (leftNew) |> toMap
            | _ -> merge (f (leftNew, nextRound)) (f (rightNew, nextRound)))

let ans2 =
    start
    |> List.windowed 2
    |> List.map (fun xs -> growBetter (xs, 40))
    |> List.fold (fun acc x -> merge acc x) Map.empty
    |> merge (Map [ (List.last start, 1) ])
    |> Map.toList
    |> List.map snd
    |> List.sort
    |> (fun xs -> (List.last xs) - (List.head xs))

printfn "%A" ans2
