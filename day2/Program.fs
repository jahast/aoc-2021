open System.IO

type Direction =
    | Up
    | Down
    | Forward

type Command = Command of Direction * int
type Position = Position of int * int

let dir (s: string) =
    match s with
    | "forward" -> Forward
    | "up" -> Up
    | "down" -> Down
    | s -> failwithf "invalid direction %s" s

let parseCommands (line: string) : Command =
    match line.Split " " with
    | [| direction; quantity |] -> Command(dir direction, int quantity)
    | _ -> failwith "invalid command"

let commandToPosition (Command (direction, quantity)) : Position =
    match direction with
    | Forward -> Position(quantity, 0)
    | Up -> Position(0, -quantity)
    | Down -> Position(0, quantity)

let input = File.ReadAllLines("./input.txt") |> Array.toList

let ans1Coordinates =
    input
    |> List.map parseCommands
    |> List.map commandToPosition
    |> List.fold (fun (Position (x0, y0)) (Position (x1, y1)) -> Position(x0 + x1, y0 + y1)) (Position(0, 0))

let ans1 =
    let Position (x, y) as _ = ans1Coordinates
    x * y

printfn "part 1: %d" ans1


type PositionWithAngle = PositionWithAngle of int * int * int

let ans2Coordinates =
    input
    |> List.map parseCommands
    |> List.fold
        (fun (PositionWithAngle (x0, y0, a0)) (Command (dir, quantity)) ->
            match dir with
            | Forward -> PositionWithAngle(x0 + quantity, y0 + quantity * a0, a0)
            | Up -> PositionWithAngle(x0, y0, a0 - quantity)
            | Down -> PositionWithAngle(x0, y0, a0 + quantity))
        (PositionWithAngle(0, 0, 0))

let ans2 =
    let PositionWithAngle (x, y, a) as _ = ans2Coordinates
    x * y

printfn "part 2: %d" ans2
