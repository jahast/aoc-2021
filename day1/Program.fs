open System.IO

let input = File.ReadAllLines("./input.txt") |> Array.toList |> List.map int

let init (list: List<_>) = list[..(list.Length - 2)]

let singeDiff = List.map2 (-) (List.tail input) (init input)

let part1Ans = singeDiff |> List.where (fun x -> x > 0) |> List.length

printfn "part 1: %d" part1Ans

let windowedSum = List.windowed 3 input |> List.map List.sum

let tripletDiff = List.map2 (-) (List.tail windowedSum) (init windowedSum)

let part2Ans = tripletDiff |> List.where (fun x -> x > 0) |> List.length

printfn "part 2: %d" part2Ans
