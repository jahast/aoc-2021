open System.Collections.Generic

let diceSum (n: int) =
    let first = (n * 3) % 100 + 1
    [ 0; 1; 2 ] |> List.map ((+) first) |> List.sum

let nextPos start roll = (start + roll - 1) % 10 + 1
let playerPos = (3, 5)
let playerPoints = (0L, 0L)

let rec round (n: int) (positions: int * int) (playerPoints: int64 * int64) =

    let p1Points, p2Points = playerPoints
    let p1Pos, p2Pos = positions

    let firstRolls = diceSum n

    let p1PosNext = nextPos p1Pos firstRolls

    let p1PointsNew = p1Points + (int64 p1PosNext)

    if p1PointsNew >= 1000L then
        p2Points * (int64 (n + 1) * 3L)
    else
        let SecondRolls = diceSum (n + 1)

        let p2PosNext = nextPos p2Pos SecondRolls

        let p2PointsNew = p2Points + (int64 p2PosNext)

        if p2PointsNew >= 1000L then
            p1PointsNew * (int64 (n + 2) * 3L)
        else
            round (n + 2) (p1PosNext, p2PosNext) (p1PointsNew, p2PointsNew)

round 0 playerPos playerPoints |> printfn "%A"


// 1,1,1; 2,2,2; 3,3,3; -> 3, 6, 9
// 1,2,3; 1,3,2; 3,1,2; 3,2,1; 2,1,3; 2,3,1-> 6, 6, 6, 6, 6, 6
// 1,1,2; 1,1,3; 1,2,1; 1,3,1; 2,1,1; 3,1,1, -> 4, 5, 4, 5, 4, 5
// 2,2,1; 2,2,3; 2,1,2; 2,3,2; 1,2,2; 3,2,2 -> 5, 7, 5, 7, 5. 7
// 3,3,1; 3,3,2; 3,1,3; 3,2,3; 1,3,3,; 2,3,3 -> 7, 8, 7, 8, 7, 8
// ->
// 1*3
// 3*4
// 6*5
// 7*6
// 6*7
// 3*8
// 1*9

let universesAndRolls =
    [ (1L, 3)
      (3L, 4)
      (6L, 5)
      (7L, 6)
      (6L, 7)
      (3L, 8)
      (1L, 9) ]

type Player =
    | P1
    | P2

let rec diracRound (nUni: int64) (p1: int * int) (p2: int * int) (p: Player) =
    let current, other = if p = P1 then p1, p2 else p2, p1
    let nextCurrent = if p = P1 then P2 else P1

    if (snd current) >= 21 then
        [ (p, nUni) ]
    else
        universesAndRolls
        |> List.map (fun (nU, roll) -> (nU, nextPos (fst other) roll))
        |> List.collect
            (fun (nU, newPos) ->
                let newP1, newP2 =
                    if p = P1 then
                        p1, (newPos, newPos + (snd other))
                    else
                        (newPos, newPos + (snd other)), p2

                diracRound (nUni * nU) newP1 newP2 nextCurrent)

let results = diracRound 1L (3, 0) (5, 0) P2

results
|> List.groupBy (fst)
|> List.map (fun (p, xs) -> (p, xs |> List.sumBy snd))
|> printfn "%A"
