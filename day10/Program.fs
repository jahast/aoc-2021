open System.IO

let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let symbolMatrix =
    input |> List.map (Seq.toList >> List.map string)

let costs =
    Map [ (")", 3)
          ("]", 57)
          ("}", 1197)
          (">", 25137) ]

let matching =
    Map [ ("(", ")")
          ("[", "]")
          ("{", "}")
          ("<", ">") ]

let openingSymbols = Map.keys matching |> List.ofSeq

let rec findIllegal (acc: string list) xs =
    let nextSymbol = List.tryHead xs

    match nextSymbol with
    | None -> 0
    | Some (x) when List.contains x openingSymbols -> findIllegal (x :: acc) (List.tail xs)
    | _ ->
        let nextPure = Option.get nextSymbol
        let currentSymbol = List.tryHead acc

        let expected =
            Option.map (fun x -> Map.find x matching) currentSymbol

        match expected with
        | Some symb when symb = nextPure -> findIllegal (List.tail acc) (List.tail xs)
        | _ -> Map.find nextPure costs

let ans1 =
    symbolMatrix
    |> List.map (findIllegal [])
    |> List.sum

printfn "%A" ans1

let costs2 =
    Map [ ("(", (int64 1))
          ("[", (int64 2))
          ("{", (int64 3))
          ("<", (int64 4)) ]

let rec calculateMissing (acc: string list) xs =
    let nextSymbol = List.tryHead xs

    match nextSymbol with
    | None ->
        acc
        |> List.fold (fun (state: int64) x -> state * (int64 5) + Map.find x costs2) (int64 0)
    | Some (x) when List.contains x openingSymbols -> calculateMissing (x :: acc) (List.tail xs)
    | _ -> calculateMissing (List.tail acc) (List.tail xs)

let ans2 =
    symbolMatrix
    |> List.filter (fun xs -> (findIllegal [] xs) |> ((=) 0))
    |> List.map (calculateMissing [])
    |> List.sort
    |> (fun xs -> xs.Item(xs.Length / 2))

printfn "%A" ans2
