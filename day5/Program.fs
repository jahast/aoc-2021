open System.IO
open System.Text.RegularExpressions

type Coordinate = Coordinate of int * int

let input = File.ReadAllLines("./input.txt") |> Array.toList

let overlappingCoords coords =
    coords
    |> List.groupBy id
    |> List.map (snd >> List.length)
    |> List.filter ((<) 1)
    |> List.length

let range (xs: list<int>) =
    let first, second = xs.Item(0), xs.Item(1)

    if first < second then
        [ first..second ]
    else
        [ first .. (-1) .. second ]

let genCoords (xs: list<int>) (ys: list<int>) =
    let rangeXs, rangeYs = range xs, range ys
    let maxLen = max rangeXs.Length rangeYs.Length

    let expand (ls: list<int>) (maxLen: int) =
        if ls.Length = 1 then
            List.replicate maxLen ls[0]
        else
            ls

    let expandedXs, expandedYs = expand rangeXs maxLen, expand rangeYs maxLen
    List.map2 (fun x y -> Coordinate(x, y)) expandedXs expandedYs

let coordinatePairs =
    input
    |> List.map (fun l ->
        Regex("\d+").Matches(l)
        |> Seq.cast<Match>
        |> Seq.map (fun i -> int i.Value)
        |> Seq.toList)

let horizontalAndVerticalCoordinates =
    coordinatePairs
    |> List.filter (fun xs -> xs.Item(0) = xs.Item(2) || xs.Item(1) = xs.Item(3))
    |> List.map (fun xs ->
        seq {
            for x in range [ xs.Item(0); xs.Item(2) ] do
                for y in range [ xs.Item(1); xs.Item(3) ] -> Coordinate(x, y)
        }
        |> List.ofSeq)
    |> List.concat


let ans1 = overlappingCoords horizontalAndVerticalCoordinates

printfn "%A" ans1

let allCoordinates =
    coordinatePairs
    |> List.map (fun xs ->
        genCoords [ xs.Item(0); xs.Item(2) ] [
            xs.Item(1)
            xs.Item(3)
        ])
    |> List.concat

let ans2 = overlappingCoords allCoordinates

printfn "%A" ans2
