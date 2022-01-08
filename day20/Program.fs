open System.IO
open Microsoft.FSharp.Collections

let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let toBin s = if s = "#" then 1 else 0

let toInt (xs: int list) =
    xs
    |> List.rev
    |> List.mapi (fun i x -> x * pown 2 i)
    |> List.sum

let toEnhancedPixel inp algo = List.item (toInt inp) algo

let split s =
    s
    |> List.ofSeq
    |> List.map string
    |> List.map toBin

let algo = input.Item(0) |> split

let image = input |> List.skip 2 |> List.map split

let neighbours x y =
    [ (x - 1, y - 1)
      (x - 1, y)
      (x - 1, y + 1)
      (x, y - 1)
      (x, y)
      (x, y + 1)
      (x + 1, y - 1)
      (x + 1, y)
      (x + 1, y + 1) ]

let printImage (xxs: int list list) =
    let output =
        xxs
        |> List.map
            (fun xs ->
                xs
                |> List.map (fun x -> if x = 1 then "#" else ".")
                |> String.concat "")
        |> String.concat "\n"

    let padded = String.concat "" [ "\n"; output; "\n" ]
    padded |> printfn "%A"


// This is slow af but I'm slightly tilted from trying to solve 19 so whatever
let rec enhance (n: int) (xs: int list list) =
    let rowLen = List.length (xs.Item(0))
    
    n |> printfn "%A"

    // better to look this up from the algo list
    let additionBit = if n % 2 = 0 then 0 else 1
    
    let rowAddition =
        List.replicate 3 (List.replicate rowLen additionBit)

    let rowsAdded = rowAddition @ xs @ rowAddition

    let transposed = rowsAdded |> List.transpose

    let colLen = List.length (transposed.Item(0))

    let colAddition =
        List.replicate 3 (List.replicate colLen additionBit)

    let colsAdded =
        colAddition @ transposed @ colAddition
        |> List.transpose

    let indicesToInspect = [ 1 .. (colLen - 2) ]

    let newImage =
        indicesToInspect
        |> List.map
            (fun x ->
                indicesToInspect
                |> List.map
                    (fun y ->
                        let orderedBits =
                            neighbours x y
                            |> List.map (fun (x, y) -> colsAdded.Item(x).Item(y))

                        let res = toEnhancedPixel orderedBits algo
                        res))

    // printImage newImage

    match n with
    | 1 -> newImage
    | _ -> enhance (n - 1) newImage

let litCount (xxs: int list list) = xxs |> List.sumBy List.sum

// printImage image

enhance 2 image |> litCount |> printfn "%A"

enhance 50 image |> litCount |> printfn "%A"


