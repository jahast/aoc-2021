open System
open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines("./input.txt") |> Array.toList

let numbers =
    input[ 0 ].Split ","
    |> Array.map (string >> int)
    |> Array.toList

let boards: list<list<int>> =
    input
    |> List.skip 2
    |> List.fold
        (fun grouped line ->
            let matches =
                Regex("\d+").Matches(line)
                |> Seq.cast<Match>
                |> Seq.map (fun i -> int i.Value)
                |> Seq.toList

            match matches.Length with
            | 0 -> [] :: grouped
            | _ -> (grouped.Head @ matches) :: grouped.Tail)
        [ [] ]
    |> List.rev

let hitIndices (board: list<int>) (hits: list<int>) =
    hits
    |> List.map (fun h -> board |> List.tryFindIndex (fun i -> i = h))
    |> List.choose id

let init (list: List<_>) = list[.. (list.Length - 2)]

let isBingo (indices: list<int>) =
    let rowNums = indices |> List.map (fun n -> n / 5)
    let columnNums = indices |> List.map (fun n -> n % 5)
    let diagonalDiffs = List.map2 (fun x y -> x - y) rowNums columnNums

    let maxSame xs =
        xs
        |> List.countBy id
        |> List.map snd
        |> fun xs -> if xs.Length = 0 then 0 else List.max xs

    match diagonalDiffs with
    | _ when maxSame rowNums = 5 -> true
    | _ when maxSame columnNums = 5 -> true
    | _ ->
        diagonalDiffs
        |> List.filter ((=) 0)
        |> List.length = 5

let notHitSum (hits: list<int>) (board: list<int>) =
    let allIndices = Set.ofSeq [ 0..24 ]
    let notHitIndices = Set.difference allIndices (Set.ofList hits)

    Set.toList notHitIndices
    |> List.map (fun i -> board[i])
    |> List.sum

let mapper (i: int) =
    let shoutedNumbers = numbers[0..i]
    let latestShoutedNumber = List.last shoutedNumbers

    let maybeBingoBoard =
        boards
        |> List.tryFind (fun b -> (hitIndices b shoutedNumbers) |> isBingo)

    match maybeBingoBoard with
    | Some b -> Some((*) (notHitSum (hitIndices b shoutedNumbers) b) latestShoutedNumber)
    | None -> None

let res1 =
    seq { 0 .. (numbers.Length - 1) }
    |> Seq.map mapper
    |> Seq.find Option.isSome

printfn $"%A{res1}"

let rec score i boards =
    let shoutedNumbers = numbers[0..i]
    let latestShoutedNumber = List.last shoutedNumbers

    let partitioned =
        boards
        |> List.partition (fun b -> (hitIndices b shoutedNumbers) |> isBingo)

    let firstBingo = partitioned |> fst |> List.tryHead
    let lengths = (fst >> List.length <| partitioned, snd >> List.length <| partitioned)

    match lengths with
    | (1, 0) ->
        (*) (notHitSum (hitIndices (Option.get firstBingo) shoutedNumbers) (Option.get firstBingo)) latestShoutedNumber
    | (_, _) -> score (i + 1) (snd partitioned)

let res2 = score 4 boards

printfn $"%A{res2}"
