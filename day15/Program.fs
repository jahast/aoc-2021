open System
open System.IO
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let matrix =
    input
    |> List.map (Seq.toList >> List.map (string >> int))

let getGoal xs =
    List.length xs - 1, List.length (xs.Item(0)) - 1

let neighbours x y goal =
    [ (1, 0); (0, 1); (-1, 0); (0, -1) ]
    |> List.map (fun (x', y') -> (x' + x, y' + y))
    |> List.filter
        (fun (x, y) ->
            let maxX, maxY = goal
            0 <= x && x <= maxX && 0 <= y && y <= maxY)

let manhattan (coord1: int * int) (coord2: int * int) =
    Math.Abs(fst coord1 - fst coord2)
    + Math.Abs(fst coord1 - fst coord2)

let rec astar
    (frontier: (int * (int * int)) list)
    (cameFrom: Map<int * int, int * int>)
    (costSoFar: Map<int * int, int>)
    (goal: int * int)
    (matrix: int list list)
    =
    let current = frontier |> List.minBy (fst) |> snd

    match current with
    | x when x = goal -> (Map.find current costSoFar, cameFrom)
    | _ ->
        let x, y = current

        let newFrontier, newCameFrom, newCostSoFar =
            (neighbours x y goal)
            |> List.filter
                (fun coord ->
                    let x', y' = coord

                    let newCost =
                        (Map.find current costSoFar)
                        + (matrix.Item(x').Item(y'))

                    let nextCost =
                        Option.defaultValue 100000000 (Map.tryFind coord costSoFar)

                    newCost < nextCost)
            |> List.fold
                (fun (frontier', cameFrom', costSoFar') coord ->
                    let x', y' = coord

                    let newCost =
                        (Map.find current costSoFar)
                        + (matrix.Item(x').Item(y'))

                    let frontierCost = newCost + manhattan current goal

                    let newFrontier = (frontierCost, coord) :: frontier'
                    let newCameFrom = Map.add coord current cameFrom'
                    let newCostSoFar = Map.add coord newCost costSoFar'
                    (newFrontier, newCameFrom, newCostSoFar))
                (frontier, cameFrom, costSoFar)

        let currentRemoved =
            newFrontier
            |> List.filter (fun x -> snd x |> (<>) current)

        astar currentRemoved newCameFrom newCostSoFar goal matrix

let goal = getGoal matrix

let ans1 =
    astar [ (0, (0, 0)) ] (Map [ (0, 0), (0, 0) ]) (Map [ (0, 0), 0 ]) goal matrix

printfn "%A" (fst ans1)

let increase value times = ((value - 1) + times) % 9 + 1

let mapMatrix times xss =
    xss
    |> List.map (fun xs -> xs |> List.map (fun x -> increase x times))

let allCols =
    let transposed = matrix |> List.transpose
    List.init 5 (fun i -> mapMatrix i transposed)
    |> List.concat
    |> List.transpose

let bigMatrix =
    List.init 5 (fun i -> mapMatrix i allCols)
    |> List.concat

let bigGoal = getGoal bigMatrix

let ans2 =
    astar [ (0, (0, 0)) ] (Map [ (0, 0), (0, 0) ]) (Map [ (0, 0), 0 ]) bigGoal bigMatrix

printfn "%A" (fst ans2)