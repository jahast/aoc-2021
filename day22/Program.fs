open System.IO
open System.Text.RegularExpressions

type Switch =
    | On
    | Off

let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let instructions =
    input
    |> List.map
        (fun s ->
            let parts = s.Split " "
            let switch = if parts.[0] = "on" then On else Off

            let ranges =
                parts.[1]
                |> Regex("-?\d+").Matches
                |> Seq.cast<Match>
                |> Seq.map (fun i -> int i.Value)
                |> List.ofSeq

            (switch, (ranges.[0], ranges.[1]), (ranges.[2], ranges.[3]), (ranges.[4], ranges.[5])))

let simpleInstructions =
    instructions
    |> List.filter
        (fun (_, (x1, x2), (y1, y2), (z1, z2)) ->
            [ x1; x2; y1; y2; z1; z2 ]
            |> List.forall (fun x -> x >= -50 && x <= 50))


let res1 =
    simpleInstructions
    |> List.fold
        (fun state i ->
            let instruction, xs, ys, zs = i

            printfn "%A" (Set.count state)

            let points =
                seq {
                    for x in (fst xs) .. (snd xs) do
                        for y in (fst ys) .. (snd ys) do
                            for z in (fst zs) .. (snd zs) -> (x, y, z)
                }
                |> Set.ofSeq

            match instruction with
            | On -> Set.union points state
            | Off -> Set.difference state points)
        Set.empty

res1 |> Set.count |> printfn "%A"

type Instruction = (Switch * (int * int) * (int * int) * (int * int))

let overlaps xs xs' =
    let x1, x2 = xs
    let y1, y2 = xs'
    x1 <= y2 && y1 <= x2

let simpleRanges xs xs' =
    let x1, x2 = xs
    let y1, y2 = xs'

    let pairs =
        [ x1; x2; y1; y2 ]
        |> List.distinct
        |> List.sort
        |> List.pairwise

    pairs
    |> List.collect
        (fun (i, j) ->
            if i = j then
                [ (i, j) ]
            else
                let rest =
                    if i + 1 = j then
                        [ (j, j) ]
                    else
                        [ (i + 1, j - 1); (j, j) ]

                [ (i, i) ] @ rest)

let isCubeIncluded cur other =
    let xs, ys, zs = cur
    let xs', ys', zs' = other

    let isIncluded ls ls' =
        (fst ls) >= (fst ls') && (snd ls) <= (snd ls')

    isIncluded xs xs'
    && isIncluded ys ys'
    && isIncluded zs zs'

let rd3 = fun (a, b, c) -> c
let nd2 = fun (a, b, c) -> b
let st1 = fun (a, b, c) -> a

let combos =
    [ (fun (a, b, c) -> (a, b)), rd3, (fun (a, b) -> (fst a, snd a, b))
      (fun (a, b, c) -> (a, c)), nd2, (fun (a, b) -> (fst a, b, snd a))
      (fun (a, b, c) -> (b, c)), st1, (fun (a, b) -> (b, fst a, snd a)) ]

let hackReduce (cubs: ((int * int) * (int * int) * (int * int)) list) combo =
    let grouped = cubs |> List.groupBy (st1 combo)

    grouped
    |> List.fold
        (fun acc (k, v) ->
            let groupLen = List.length v

            match groupLen with
            | 1 -> acc |> List.append v
            | _ ->
                let rec pairwiseReduce cubs =

                    let toMaybeCombine, rest =
                        cubs
                        |> List.sortBy ((nd2 combo) >> fst)
                        |> List.chunkBySize 2
                        |> List.partition (fun x -> (List.length x) = 2)

                    let newCubs =
                        toMaybeCombine
                        |> List.map (fun x -> (x.[0], x.[1]))
                        |> List.collect
                            (fun (a, b) ->
                                let aDim, bDim = (nd2 combo) a, (nd2 combo) b
                                let (x1, x2) = aDim
                                let isOverlap = overlaps (x1, x2 + 1) bDim

                                match isOverlap with
                                | false -> [ a; b ]
                                | true ->
                                    let min = [ (fst aDim); (fst bDim) ] |> List.min
                                    let max = [ (snd aDim); (snd bDim) ] |> List.max
                                    let newRange = (min, max)
                                    let ret = [ (rd3 combo) (k, newRange) ]
                                    ret)

                    let restFlattened = List.concat rest

                    match (((List.length toMaybeCombine) * 2) = (List.length newCubs)) with
                    | true -> newCubs @ restFlattened
                    | false -> pairwiseReduce (newCubs @ restFlattened)

                let reduced = pairwiseReduce v

                acc |> List.append reduced

            )
        []

let megaReduce xs =
    combos
    |> List.fold (fun state x -> hackReduce state x) xs

// other is always "On"
let filterOffsAndInvalids (cub: (int * int) * (int * int) * (int * int)) next other =
    let xs, ys, zs = next

    let nextAsRange = (xs, ys, zs)

    let isIncludedInNext = isCubeIncluded cub nextAsRange

    if isIncludedInNext then
        false
    else
        let isIncludedInOther = isCubeIncluded cub other

        match isIncludedInNext, isIncludedInOther with
        | false, false -> false
        | true, false -> false
        | false, true -> true
        | true, true -> false

// other is always "On"
let sliceNDice next other =
    let xs, ys, zs = next
    let xs', ys', zs' = other

    let doesOverlap =
        overlaps xs xs'
        && overlaps ys ys'
        && overlaps zs zs'

    match doesOverlap with
    | false -> [ other ]
    | true ->

        let xRanges, yRanges, zRanges =
            simpleRanges xs xs', simpleRanges ys ys', simpleRanges zs zs'

        let allSlices =
            zRanges
            |> List.allPairs (List.allPairs xRanges yRanges)
            |> List.map (fun (a, b) -> (fst a, snd a, b))
            |> List.distinct


        let newSlices =
            allSlices
            |> List.filter (fun cub -> filterOffsAndInvalids cub next other)

        newSlices

let rang xs =
    ((snd >> int64) xs) - ((fst >> int64) xs) + 1L

let on xs =
    xs
    |> List.sumBy (fun (xs, ys, zs) -> (rang xs) * (rang ys) * (rang zs))



// assume first instruction is "On"
let instr = instructions
let _, a, b, c = instr.[0]
let toProcess = List.tail instr

let slicedAndDiced =
    toProcess
    |> List.fold
        (fun state i ->
            printfn "%A" (on state)
            printfn "%A" i

            let (origI, a, b, c) = i

            let newInstruction = (a, b, c)

            let offed =
                state
                |> List.fold
                    (fun innerState j ->

                        let replacement = sliceNDice newInstruction j

                        let newState =
                            innerState
                            |> List.append replacement
                            |> megaReduce

                        newState)
                    []

            if origI = On then
                newInstruction :: offed
            else
                offed)

        [ (a, b, c) ]

let ans2 = on slicedAndDiced

ans2 |> printfn "%A"
