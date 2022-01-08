open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

// I slightly tilted with this one

// absolutely terrible
let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let coords =
    input
    |> List.filter (fun (s: string) -> not (s.StartsWith("---")))
    |> List.fold
        (fun acc s ->
            match s with
            | "" -> [] :: acc
            | line ->
                let matches =
                    Regex("-?\d+").Matches(line)
                    |> Seq.cast<Match>
                    |> Seq.map (fun i -> int i.Value)
                    |> Seq.toList

                let newHead = matches :: (List.head acc)
                newHead :: (List.skip 1 acc))
        [ [] ]
    |> List.rev

let manhattan xs ys =
    List.map2 (fun (x: int) (y: int) -> Math.Abs(x - y)) xs ys
    |> List.sum

// self-invented name
let structuralDistance xs ys =
    xs
    |> List.map2 (fun (x: int) (y: int) -> Math.Abs(x - y)) ys
    |> List.sort
    |> fun ls -> (ls.Item(0), ls.Item(1), ls.Item(2))

let componentDistance xs ys =
    xs
    |> List.map2 (fun (x: int) (y: int) -> x - y) ys
    |> fun ls -> (ls.Item(0), ls.Item(1), ls.Item(2))

let rec comb n l =
    match n, l with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | k, x :: xs -> List.map ((@) [ x ]) (comb (k - 1) xs) @ comb k xs

let pointToPointDists =
    coords
    |> List.map
        (fun xs ->
            xs
            |> List.indexed
            |> comb 2
            |> List.map
                (fun ls ->
                    let (i, x), (j, y) = ls.Item(0), ls.Item(1)
                    i, j, structuralDistance x y))

let overlappingPointsPerScanner =
    pointToPointDists
    |> List.indexed
    |> comb 2
    |> List.choose
        (fun ls ->
            let (i, xs), (j, ys) = ls.Item(0), ls.Item(1)
            let yDists = ys |> List.map (fun (_, _, d) -> d)
            let xDists = xs |> List.map (fun (_, _, d) -> d)

            let includedInYs =
                xs
                |> List.filter (fun (_, _, d) -> List.contains d yDists)
                |> List.map (fun (x, y, _) -> (x, y))


            let includedInXs =
                ys
                |> List.filter (fun (_, _, d) -> List.contains d xDists)
                |> List.map (fun (x, y, _) -> (x, y))

            let xPoints =
                (List.map fst includedInYs)
                @ (List.map snd includedInYs)
                |> set

            let yPoints =
                (List.map fst includedInXs)
                @ (List.map snd includedInXs)
                |> set

            if (Set.count xPoints) >= 12 then
                Some((i, j, xPoints, yPoints))
            else
                None)

let xNormal =
    [ [ 1; 2; 3 ]
      [ 1; 3; -2 ]
      [ 1; -2; -3 ]
      [ 1; -3; 2 ] ]

let xPointsUp =
    [ [ -3; 2; 1 ]
      [ 2; 3; 1 ]
      [ 3; -2; 1 ]
      [ -2; -3; 1 ] ]

let xPointsLeft =
    [ [ -2; 1; 3 ]
      [ 3; 1; 2 ]
      [ 2; 1; -3 ]
      [ -3; 1; -2 ] ]

let xPointsRight =
    [ [ 2; -1; 3 ]
      [ -3; -1; 2 ]
      [ -2; -1; -3 ]
      [ 3; -1; -2 ] ]

let xPointsDown =
    [ [ 3; 2; -1 ]
      [ 2; -3; -1 ]
      [ -3; -2; -1 ]
      [ -2; 3; -1 ] ]

let xPointsBackwards =
    [ [ -1; -2; 3 ]
      [ -1; 3; 2 ]
      [ -1; 2; -3 ]
      [ -1; -3; -2 ] ]

let coordinateTransformations =
    xNormal
    @ xPointsUp
      @ xPointsBackwards
        @ xPointsDown @ xPointsLeft @ xPointsRight

let makeFun (ls: int list) =
    fun (xs: int list) ->
        ls
        |> List.map
            (fun x ->
                let mult = if x < 0 then -1 else 1
                let idx = Math.Abs x - 1
                let el = xs.Item(idx)
                mult * el)

let mappingFuns =
    coordinateTransformations |> List.map makeFun

let totalOverlappingScanners =
    (List.map (fun (i, _, _, _) -> i) overlappingPointsPerScanner)
    @ (List.map (fun (_, j, _, _) -> j) overlappingPointsPerScanner)
    |> set
    |> Set.count

let mutable scannerPositions = Map [ (0, [ 0; 0; 0 ]) ]
let mutable scannerMappingfuns: Map<int, int list -> int list> = Map [ 0, id ]
let mutable allPoints = set coords.[0]

while (Map.count scannerPositions) < totalOverlappingScanners do
    let nextPair =
        overlappingPointsPerScanner
        |> List.pick
            (fun s ->
                let i, j, iPointIdxs, jPointIdxs = s

                let iKnown, jKnown =
                    Map.containsKey i scannerPositions, Map.containsKey j scannerPositions

                match iKnown, jKnown with
                | false, false -> None
                | true, true -> None
                | true, false -> Some(s)
                | false, true -> Some(j, i, jPointIdxs, iPointIdxs))

    let i, j, iPointIdxs, jPointIdxs = nextPair

    let iMappingFun = Map.find i scannerMappingfuns

    let iPoints =
        iPointIdxs
        |> Set.toList
        |> List.map (fun k -> coords.Item(i).Item(k))
        |> List.map iMappingFun
        |> List.sort

    let jPoints =
        jPointIdxs
        |> Set.toList
        |> List.map (fun k -> coords.Item(j).Item(k))
        |> List.sort

    let iCompDists =
        iPoints
        |> List.sort
        |> comb 2
        |> List.map (fun ls -> componentDistance (ls.Item(0)) (ls.Item(1)))

    let correctFun, constant =
        mappingFuns
        |> List.pick
            (fun f ->
                let mappedPoints = List.map f jPoints |> List.sort

                let jCompDists =
                    mappedPoints
                    |> comb 2
                    |> List.map (fun ls -> componentDistance (ls.Item(0)) (ls.Item(1)))

                let matches = set jCompDists = set iCompDists

                match matches with
                | false -> None
                | true ->
                    let iPoint, jPoint = iPoints.[0], mappedPoints.[0]
                    let constant = List.map2 (-) iPoint jPoint
                    Some(f, constant))

    let prevConst = Map.find i scannerPositions
    let newConst = List.map2 (+) constant prevConst

    let newCoords =
        coords.[j]
        |> List.map correctFun
        |> List.map (List.map2 (+) newConst)

    scannerMappingfuns <- scannerMappingfuns.Add(j, correctFun)
    scannerPositions <- scannerPositions.Add(j, newConst)
    allPoints <- Set.union allPoints (set newCoords)

Set.count allPoints |> printfn "%A"

let ans2 =
    scannerPositions
    |> Map.values
    |> List.ofSeq
    |> comb 2
    |> List.map
        (fun ls ->
            let xs, ys = ls.[0], ls.[1]
            manhattan xs ys)
    |> List.max

ans2 |> printfn "%A"
