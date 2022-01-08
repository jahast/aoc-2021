open System
open System.IO


let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let pathTuples =
    input
    |> List.map
        (fun x ->
            x.Split("-", StringSplitOptions.TrimEntries)
            |> List.ofArray)
    |> List.map (fun xs -> (xs.Item(0), xs.Item(1)))

let inverted =
    pathTuples |> List.map (fun (i, j) -> (j, i))

let pathMap =
    pathTuples @ inverted
    |> List.groupBy fst
    |> List.map (fun (k, v) -> (k, List.map snd v))
    |> Map

let rec traverse (path: string list) =
    let pos = List.head path

    match pos with
    | "end" -> 1
    | _ ->

        let traversedSmallCaves =
            path |> List.filter (fun x -> (x.ToLower()) = x)

        let possibleNextCaves =
            Map.find pos pathMap
            |> List.except ("start" :: traversedSmallCaves)

        if List.length possibleNextCaves = 0 then
            0
        else
            List.map (fun cave -> traverse (cave :: path)) possibleNextCaves
            |> List.sum

let ans1 = traverse [ "start" ]

printfn "%A" ans1

let rec traverse2 (path: string list) =
    let pos = List.head path

    match pos with
    | "end" -> 1
    | _ ->

        let traversedSmallCaves =
            path |> List.filter (fun x -> (x.ToLower()) = x)

        let anySmallCaveTraversedTwice =
            traversedSmallCaves
            |> List.groupBy id
            |> List.exists (snd >> List.length >> (=) 2)

        let smallCaveExclusion =
            "start"
            :: (if anySmallCaveTraversedTwice then
                    traversedSmallCaves
                else
                    [])

        let possibleNextCaves =
            Map.find pos pathMap
            |> List.except smallCaveExclusion

        if List.length possibleNextCaves = 0 then
            0
        else
            List.map (fun cave -> traverse2 (cave :: path)) possibleNextCaves
            |> List.sum

let ans2 = traverse2 [ "start" ]

printfn "%A" ans2
