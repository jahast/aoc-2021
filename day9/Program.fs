open System.IO

let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let matrix =
    input
    |> List.map (fun x -> x |> Seq.toList |> List.map (string >> int))

let transposed = List.transpose matrix

let potentialLowPoints (xs: list<int>) =
    xs
    |> List.mapi
        (fun i x ->
            [ List.tryItem (i - 1) xs
              List.tryItem (i + 1) xs ]
            |> List.choose id
            |> List.forall ((<) x))

let horizontalLows, verticalLows =
    matrix |> List.map potentialLowPoints,
    transposed
    |> List.map potentialLowPoints
    |> List.transpose

let lowPoints =
    List.map2 (List.map2 (&&)) horizontalLows verticalLows

let lowPointCoords =
    lowPoints
    |> List.mapi (fun i -> List.mapi (fun j y -> if y then Some((i, j)) else None))
    |> List.concat
    |> List.choose id

let ans1 =
    lowPointCoords
    |> List.map (fun (x, y) -> matrix.Item(x).Item(y))
    |> List.map ((+) 1)
    |> List.sum

printfn "%A" ans1

let rec basin (matrix: int list list) (x: int) (y: int) (visited: (int * int) list) =
    let coordVal =
        List.tryItem x matrix
        |> Option.bind (List.tryItem y)

    match coordVal with
    | None -> visited
    | Some (9) -> visited
    | Some _ ->
        let newVisited = (x, y) :: visited

        [ x - 1, y
          x + 1, y
          x, y - 1
          x, y + 1 ]
        |> List.filter (fun c -> not (List.contains c visited))
        |> List.fold (fun xs (x, y) -> List.distinct (xs @ basin matrix x y xs)) newVisited

let ans2 =
    lowPointCoords
    |> List.map (fun (x, y) -> basin matrix x y [])
    |> List.map List.length
    |> List.sortDescending
    |> List.take 3
    |> List.fold (*) 1

printfn "%A" ans2
