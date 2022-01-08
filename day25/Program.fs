open System.IO

let split s = s |> List.ofSeq |> List.map string

let input =
    File.ReadAllLines("./input.txt")
    |> Array.toList
    |> List.map split

let step (cucu: string) (xss: string list list) =
    let len = List.length xss.[0]

    let mutable somethingMoved = false

    let ret =
        xss
        |> List.map
            (fun xs ->
                let newIds =
                    xs
                    |> List.indexed
                    |> List.choose
                        (fun (i, x) ->
                            match x with
                            | "." -> None
                            | x when x = cucu ->
                                let nextIdx = (i + 1) % len
                                let next = xs.[nextIdx]

                                match next with
                                | "." ->
                                    somethingMoved <- true
                                    Some(nextIdx, x)
                                | _ -> Some(i, x)

                            | x -> Some(i, x))

                newIds
                |> List.sortBy fst
                |> List.fold
                    (fun state (idx, char) ->
                        let itemsNow = List.length state
                        
                        let emptys =
                            if idx = 0 then
                                []
                            else
                                List.replicate (idx - itemsNow) "."

                        state @ emptys @ [ char ])
                    []
                |> (fun xs ->
                    let curLen = List.length xs

                    if curLen = len then
                        xs
                    else
                        xs @ (List.replicate (len - curLen) "."))


                )

    ret, somethingMoved

let print xss =
    xss
    |> List.map (String.concat "")
    |> List.insertAt 0 ""
    |> List.append [ "" ]
    |> String.concat "\n"
    |> printfn "%A"

let rec solve round xss =
    printfn "%A" round
    //print xss
    let eastMovement, eMoved = step ">" xss

    //print eastMovement

    let southMovement, sMoved =
        eastMovement |> List.transpose |> step "v"

    let next = List.transpose southMovement

    //print next

    match eMoved, sMoved with
    | false, false -> round
    | _, _ -> solve (round + 1) next


solve 1 input |> printfn "%A"
