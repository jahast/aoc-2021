open System

let xRange = [ 192 .. 251 ]
let yRange = [ -89 .. -59 ]

// these are the possible ys when the probe is shot upwrads
let possibleYs =
    yRange |> List.map ((+) 1) |> List.map Math.Abs

let nearestX = List.head xRange |> float
let furthestX = List.last xRange |> float

// second degree polynomial (where n > 0) and summation formula for 1..n
// n^2 + n -2x >= 0
let solve x =
    ((float -1) + Math.Sqrt((float 1) + (float 8) * x))
    / (float 2)

// n < 0
let solveNeg x =
    ((float -1) - Math.Sqrt((float 1) + (float 8) * x))
    / (float 2)

let firstPossibleX = Math.Ceiling(solve nearestX)
let lastPossibleX = Math.Floor(solve furthestX)

// with these xs the probe will reach
// an x velocity of zero
let possibleXs =
    [ firstPossibleX .. lastPossibleX ]
    |> List.map int

let sum i = i * (i + 1) / 2

let steps x y =
    Seq.initInfinite
        (fun i ->
            (List.init i (fun n -> Math.Max(0, x - n))
             |> List.sum,
             List.init i (fun n -> y - n) |> List.sum))

let find searchXs searchYs landingXs landingYs =
    let rec inner n (stepGenerator: (int * int) seq) =
        let currentStep = Seq.item n stepGenerator

        match currentStep with
        | (a, b) when
            List.exists ((=) a) landingXs
            && List.exists ((=) b) landingYs
            ->
            Some(n)
        | (_, b) when b < (List.min landingYs) -> None
        | _ -> inner (n + 1) stepGenerator

    searchYs
    |> List.pick
        (fun y ->
            searchXs
            |> List.tryPick
                (fun x ->
                    let stepGen = steps x y
                    let res = inner 1 stepGen

                    match res with
                    | Some round ->
                        stepGen
                        |> Seq.take round
                        |> Seq.map (fun coords -> snd coords)
                        |> Seq.max
                        |> Some
                    | None -> None))

let ans1 = find possibleXs possibleYs xRange yRange

printfn "%A" ans1

let xUpperLimit = List.last xRange
let xLowerLimit = firstPossibleX

let xRangeAll =
    [ xLowerLimit .. xUpperLimit ] |> List.map int

let yUpperLimit = List.min yRange |> ((+) 1) |> Math.Abs
let yLowerLimit = List.min yRange
let yRangeAll = [ yLowerLimit .. yUpperLimit ]


// too lazy to refactor
let findAll searchXs searchYs landingXs landingYs =
    let rec inner n (stepGenerator: (int * int) seq) =
        let currentStep = Seq.item n stepGenerator

        match currentStep with
        | (a, b) when
            List.exists ((=) a) landingXs
            && List.exists ((=) b) landingYs
            ->
            Some(n)
        | (_, b) when b < (List.min landingYs) -> None
        | _ -> inner (n + 1) stepGenerator

    searchYs
    |> List.map
        (fun (y: int) ->
            searchXs
            |> List.choose
                (fun (x: int) ->
                    let stepGen = steps x y
                    let res = inner 1 stepGen

                    match res with
                    | Some _ -> Some(x, y)
                    | None -> None))

    |> List.concat

let ans2 =
    findAll xRangeAll yRangeAll xRange yRange

printfn "%A" (List.length ans2)
