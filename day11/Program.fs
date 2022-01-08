open System.IO

let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let matrix =
    input
    |> List.map (Seq.toList >> List.map (string >> int))

let flat = matrix |> List.concat

let neighbours x =
    let row, col = x / 10, x % 10

    List.allPairs [ 0; 1; -1 ] [ 0; -1; 1 ]
    |> List.tail
    |> List.map (fun (x, y) -> x + row, y + col)
    |> List.filter (fun (x, y) -> 0 <= x && x <= 9 && 0 <= y && y <= 9)
    |> List.map (fun (x, y) -> x * 10 + y)

let updateAt (index: int) (f: 'A -> 'A) (xs: 'A list) =
    let newItem = f (xs.Item(index))
    List.updateAt index newItem xs

let rec flash acc xs fCount =
    let allFlashes = xs |> List.map ((<) 9)
    let newFlashes = List.map2 (<>) allFlashes acc

    let newFlashedLength =
        newFlashes |> List.filter id |> List.length

    match newFlashedLength with
    | 0 -> xs, fCount
    | _ ->
        let flashAdditions =
            newFlashes
            |> List.mapi (fun i x -> if x then neighbours i else [])
            |> List.concat
            |> List.groupBy id
            |> List.map (fun (k, v) -> k, (List.length v))
            |> Map

        let additionVector =
            [ 0 .. 99 ]
            |> List.map
                (fun x ->
                    flashAdditions
                    |> Map.tryFind x
                    |> Option.defaultValue 0)

        let newXs = List.map2 (+) xs additionVector

        flash allFlashes newXs (fCount + newFlashedLength)

let rec step (round: int) (xs: int list) (fCount: int) =
    let incremented = xs |> List.map ((+) 1)

    let flashed, newFCount =
        flash (List.replicate 100 false) incremented 0

    let newXs =
        flashed
        |> List.map (fun x -> if x > 9 then 0 else x)

    let nextRound, totalFCount = round - 1, fCount + newFCount

    match nextRound with
    | 0 -> newXs, totalFCount
    | _ -> step nextRound newXs totalFCount

let xs, ans1 = step 100 flat 0

printfn "%A" ans1

let rec step2 (round: int) (xs: int list) =
    let incremented = xs |> List.map ((+) 1)

    let flashed, newFCount =
        flash (List.replicate 100 false) incremented 0

    let newXs =
        flashed
        |> List.map (fun x -> if x > 9 then 0 else x)

    let nextRound = round + 1

    match newFCount with
    | 100 -> nextRound
    | _ -> step2 nextRound newXs

let ans2 = step2 0 flat

printfn "%A" ans2
