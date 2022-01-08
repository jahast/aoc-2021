open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Core

// This is a hacky intuition based approach
// not diligent enough to write an explanation but
// you basically either accept of don't accept the
// equation the thing throws at you. If you accept all
// the ones that are possible i.e. all w < 10
// you get the correct constraints. Then you solve on paper

let input =
    File.ReadAllLines("./input.txt")
    |> Array.toList
    |> List.map (fun l -> l.Split " " |> List.ofSeq)

// w,x,y,z, input index

// lol
let simpleSolve (inp: string) =
    let str = inp.Replace("(25 + 1)", "26")

    if str.Contains("%") then

        let groups =
            Regex
                .Match(
                    str,
                    "(.* \* 26\) \+)(.*)(\% 26)"
                )
                .Groups

        let whatWeWant = (groups.Item 2).Value

        let whatWeReallyWant =
            if whatWeWant = "" then
                let endIdx = str.IndexOf(")")
                str.Substring(0, endIdx)
            else
                whatWeWant

        whatWeReallyWant
            .Replace("(", "")
            .Replace(")", "")
            .Trim()

    elif str.Contains("/") then
        let lastTermStart = str.LastIndexOf("(")
        let lastTermPlusCruft = str.Substring(lastTermStart)
        let lastTermEnd = lastTermPlusCruft.IndexOf(")")

        let lastTerm =
            lastTermPlusCruft.Substring(1, lastTermEnd - 1)

        let theNumber =
            Regex.Match(lastTerm, "\d+").Value |> int

        let omg =
            if theNumber > 16 then
                printfn "oh noes"
                System.Console.ReadLine()
                None
            else
                None

        if lastTermStart - 9 < 0 then
            printfn "%A" str
            System.Console.ReadLine()
        else

            let goodStuff = str.Substring(0, lastTermStart - 9)
            let badStuff = str.Substring(lastTermStart)

            let count x = Seq.filter ((=) x) >> Seq.length

            let toTrim = count ')' badStuff

            goodStuff.Substring(toTrim).Trim()
    else
        str

let tryParse (x: string) =
    match System.Int32.TryParse x with
    | true, int -> Some int
    | _ -> None

let maybeGetFromState toGet state =
    let w, x, y, z, _ = state

    match toGet with
    | "z" -> z
    | "w" -> w
    | "y" -> y
    | "x" -> x
    | x -> x

let makeNewState place value state =
    let simplified = state
    let w, x, y, z, idx = simplified

    match place with
    | "z" -> w, x, y, value, idx
    | "w" -> value, x, y, z, idx
    | "y" -> w, x, value, z, idx
    | "x" -> w, value, y, z, idx

let format (str: string) =
    let hasSpaces = str.Contains(" ")

    let endsAreGood = str.StartsWith("(") && str.EndsWith(")")

    let len = str.Length

    let midPart =
        if len < 3 then
            ""
        else
            str.Substring(1, len - 2)

    let hasDoubleClosures =
        midPart.StartsWith("(") && midPart.EndsWith(")")

    match hasSpaces, endsAreGood, hasDoubleClosures with
    | true, false, _ -> "(" + str + ")"
    | true, true, true -> str
    | true, true, false -> "(" + str + ")"
    | _ -> str

let stringState = ("0", "0", "0", "0", 13)

let figureItOut =
    input
    |> List.indexed
    |> List.fold
        (fun state (idx, i) ->
            printfn "%A" state
            printfn "%A" idx
            printfn "%A" i

            match i.[0] with
            | "inp" ->
                let _, x, y, z, inputIdx = state
                let inputVar = "w" + (string inputIdx)
                let nextIdx = inputIdx - 1
                (inputVar, x, y, z, nextIdx)
            | "mul" ->
                let place = i.[1]
                let toMult = maybeGetFromState i.[1] state
                let multiplier = maybeGetFromState i.[2] state

                let newVal =
                    if toMult = "0" || multiplier = "0" then
                        "0"
                    else if multiplier = "1" then
                        toMult
                    else
                        (format toMult) + " * " + (format multiplier)

                makeNewState place newVal state

            | "add" ->
                let place = i.[1]
                let toAdd = maybeGetFromState i.[1] state
                let addition = maybeGetFromState i.[2] state

                let newVal =
                    if addition = "0" then
                        toAdd
                    else if toAdd = "0" then
                        addition
                    else
                        (format toAdd) + " + " + (format addition)

                makeNewState place newVal state

            | "div" ->
                let place = i.[1]
                let toDiv = maybeGetFromState i.[1] state
                let divisor = maybeGetFromState i.[2] state

                let newVal =
                    if divisor = "1" then
                        toDiv
                    else
                        let maybe =
                            (format toDiv) + " / " + (format divisor)

                        simpleSolve maybe

                makeNewState place newVal state

            | "eql" ->
                let place = i.[1]
                let left = maybeGetFromState i.[1] state
                let right = maybeGetFromState i.[2] state

                let leftInt, rightInt = tryParse left, tryParse right

                let newVal =
                    match leftInt, rightInt with
                    | Some l, Some r -> if l = r then "1" else "0"
                    | _ ->
                        let maybe = (format left) + " = " + (format right)

                        maybe

                makeNewState place newVal state

            | "mod" ->
                let place = i.[1]
                let toMod = maybeGetFromState i.[1] state
                let modd = maybeGetFromState i.[2] state

                let newVal =
                    if toMod = "0" then
                        "0"
                    else
                        let maybe = (format toMod) + " % " + (format modd)
                        simpleSolve maybe

                makeNewState place newVal state)
        stringState

printfn "%A" figureItOut


// let w, x, y, z, idx = simplified
