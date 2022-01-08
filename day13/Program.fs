open System
open System.IO
open System.Text.RegularExpressions

let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let coords =
    input
    |> List.takeWhile (fun x -> x <> "")
    |> List.map
        (fun x ->
            x
            |> Regex("\d+").Matches
            |> Seq.cast<Match>
            |> Seq.map (fun i -> int i.Value)
            |> List.ofSeq
            |> (fun x -> (x.Item(0), x.Item(1))))

let folds =
    input
    |> List.skipWhile (fun x -> not (x.Contains("fold")))
    |> List.map (fun x -> x.Substring(11))
    |> List.map
        (fun x ->
            x.Split("=")
            |> List.ofArray
            |> (fun x -> (x.Item(0), int (x.Item(1)))))

let maxRows =
    folds
    |> List.find (fst >> ((=) "y"))
    |> fun (_, v) -> v * 2 + 1

let maxCols =
    folds
    |> List.find (fst >> ((=) "x"))
    |> fun (_, v) -> v * 2 + 1

let matrix =
    List.init maxRows (fun i -> List.init maxCols (fun j -> false))

let dotted =
    coords
    |> List.fold
        (fun (matr: bool list list) (col, row) ->
            let rowToUpdate = matr.Item(row)
            let updated = List.updateAt col true rowToUpdate
            matr |> List.updateAt row updated)
        matrix

let foldVertical (i: int) (xs: bool list list) =
    let first, second = xs |> List.removeAt i |> List.splitAt i
    let mirrored = List.rev second
    List.map2 (List.map2 (||)) first mirrored

let foldHorizontal (j: int) (xs: bool list list) =
    let first, second =
        xs
        |> List.transpose
        |> List.removeAt j
        |> List.splitAt j

    let mirrored = List.rev second

    List.map2 (List.map2 (||)) first mirrored
    |> List.transpose

let foldFolds fs =
    fs
    |> List.fold
        (fun xs (dir, idx) ->
            match dir with
            | "x" -> foldHorizontal idx xs
            | _ -> foldVertical idx xs)
        dotted

let ans1 =
    foldFolds [ List.head folds ]
    |> List.map (fun xs -> xs |> List.filter id |> List.length)
    |> List.sum

printfn "%A" ans1

let ans2 = foldFolds folds

let vis =
    ans2
    |> List.map
        (fun xs ->
            xs
            |> List.map (fun x -> if x then "#" else ".")
            |> String.Concat)

let padded = ("" :: vis) @ [ "" ]
let ans = String.Join("\n", padded)

printfn "%A" ans
