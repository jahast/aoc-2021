open System
open System.IO

let input =
    File.ReadAllLines("./input.txt") |> Array.toList

let cleanSplit (sep: string) (s: string) =
    s.Split(
        sep,
        (StringSplitOptions.TrimEntries
         ||| StringSplitOptions.TrimEntries)
    )
    |> List.ofArray

let parsed =
    input
    |> List.map
        (fun x ->
            let splat = x |> cleanSplit "|"

            let firstPart =
                splat.Item(0)
                |> cleanSplit " "
                |> List.map (fun (x: string) -> x |> Seq.toList |> List.map string)

            let secondPart =
                splat.Item(1)
                |> cleanSplit " "
                |> List.map (fun (x: string) -> x |> Seq.toList |> List.map string)

            (firstPart, secondPart))


let ans1 =
    parsed
    |> List.map snd
    |> List.map
        (fun xs ->
            let lens = xs |> List.map List.length

            lens
            |> List.filter (fun x -> List.contains x [ 2; 3; 4; 7 ])
            |> List.length)
    |> List.sum

printfn "%A" ans1

let segmentsToDigits xs =
    let sorted = List.sort xs

    match sorted with
    | [ 1; 2; 3; 5; 6; 7 ] -> 0
    | [ 3; 6 ] -> 1
    | [ 1; 3; 4; 5; 7 ] -> 2
    | [ 1; 3; 4; 6; 7 ] -> 3
    | [ 2; 3; 4; 6 ] -> 4
    | [ 1; 2; 4; 6; 7 ] -> 5
    | [ 1; 2; 4; 5; 6; 7 ] -> 6
    | [ 1; 3; 6 ] -> 7
    | [ 1; 2; 3; 4; 5; 6; 7 ] -> 8
    | [ 1; 2; 3; 4; 6; 7 ] -> 9
    | c -> failwithf $"invalid segments %A{c}"

let figureItOut (xs: list<list<string>>, digits: list<list<string>>) =
    let getByLength ls (len: int) =
        ls
        |> List.filter (fun xxs -> List.length xxs = len)

    let getOneByLength ls (len: int) = getByLength ls len |> List.exactlyOne

    let diff xs ys =
        Set.difference (Set.ofList xs) (Set.ofList ys)
        |> Set.toList

    let intersection xs ys =
        Set.intersect (Set.ofList xs) (Set.ofList ys)
        |> Set.toList

    let exactlyN n xs =
        let len = List.length xs

        match len with
        | len when len = n -> xs
        | len -> failwithf $"tried getting exactly %A{len} from %A{xs}"

    let seven = getOneByLength xs 3
    let one = getOneByLength xs 2
    let four = getOneByLength xs 4

    let segment1 = diff seven one |> List.exactlyOne
    let segments24 = diff four one |> exactlyN 2

    let twoThreeAndFive = getByLength xs 5

    let segments25 =
        twoThreeAndFive
        |> List.concat
        |> List.countBy id
        |> List.filter (fun x -> snd x = 1)
        |> List.map fst

    let segment2, segment5, segment4 =
        let s2 =
            intersection segments24 segments25
            |> List.exactlyOne

        let s5 =
            diff segments25 [ s2 ] |> List.exactlyOne

        let s4 =
            diff segments24 [ s2 ] |> List.exactlyOne

        s2, s5, s4

    let two =
        twoThreeAndFive
        |> List.filter (List.contains segment5)
        |> List.exactlyOne

    let segment3 = intersection one two |> List.exactlyOne

    let segment6 =
        diff seven [ segment1; segment3 ]
        |> List.exactlyOne

    let segment7 =
        diff
            two
            [ segment1
              segment3
              segment4
              segment5 ]
        |> List.exactlyOne

    let segmentMap =
        Map [ (segment1, 1)
              (segment2, 2)
              (segment3, 3)
              (segment4, 4)
              (segment5, 5)
              (segment6, 6)
              (segment7, 7) ]

    let mapped =
        digits
        |> List.map (fun ls -> ls |> List.map (fun x -> Map.find x segmentMap))
        |> List.map segmentsToDigits

    let res =
        mapped
        |> List.map string
        |> String.concat ""
        |> int

    res

let ans2 =
    parsed |> List.map figureItOut |> List.sum

printfn "%A" ans2
