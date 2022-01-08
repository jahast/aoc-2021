open System.IO

let input = File.ReadAllLines("./input.txt") |> Array.toList

let numberList (str: string) =
    str
    |> Seq.toList
    |> Seq.map string
    |> Seq.map int
    |> Seq.toList

let binaryMatrix = input |> List.map numberList

let gammaBinary =
    binaryMatrix
    |> List.transpose
    |> List.map (fun seq ->
        if (List.sum seq) * 2 > (List.length seq) then
            1
        else
            0)

let flip (seq: list<int>) : list<int> =
    seq
    |> List.map (fun x ->
        match x with
        | 1 -> 0
        | 0 -> 1
        | a -> failwithf "invalid bit %d" a)

let epsilonBinary = flip gammaBinary

let toInt (seq: list<int>) : int =
    seq
    |> List.rev
    |> List.mapi (fun i x -> x * pown 2 i)
    |> List.sum

let gamma, episolon = toInt gammaBinary, toInt epsilonBinary

let ans1 = gamma * episolon

printfn "%d" ans1

let mode (seq: list<int>) : int =
    let sum, len = List.sum seq, List.length seq

    match sum with
    | _ when sum * 2 < len -> 0
    | _ when sum * 2 > len -> 1
    | _ -> 1

let antiMode (seq: list<int>) : int =
    let sum, len = List.sum seq, List.length seq

    match sum with
    | _ when sum * 2 > len -> 0
    | _ when sum * 2 < len -> 1
    | _ -> 0

let rec bitCriteria matrix i modefun =
    let column = matrix |> List.map (List.item i)
    let currentMode = modefun column

    let newMatrix =
        matrix
        |> List.filter (fun xs -> (List.item i xs) = currentMode)

    match newMatrix.Length with
    | 1 -> newMatrix[0]
    | _ -> bitCriteria newMatrix (i + 1) modefun

let oxygenBinary = bitCriteria binaryMatrix 0 mode

let co2Binary = bitCriteria binaryMatrix 0 antiMode

let ans2 = (toInt oxygenBinary) * (toInt co2Binary)

printfn "%d" ans2
