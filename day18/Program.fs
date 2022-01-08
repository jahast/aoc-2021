open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Core
// first time I used mutable here :(

type Node =
    | Tip of int
    | Node of Node * Node

type Bubble =
    | LeftBubble of int
    | RightBubble of int
    | NoBubble

let explodeString (str: string) = str |> List.ofSeq |> List.map string

let input =
    File.ReadAllLines("./input.txt")
    |> Array.toList
    |> List.map explodeString

let rec parse (str: string list) : Node =
    let rec inner s =
        let next, rest = List.head s, List.skip 1 s

        match next with
        | "[" ->
            let firstNode, afterFirst = inner rest
            let secondNode, afterSecond = inner afterFirst
            Node(firstNode, secondNode), afterSecond
        | s when Regex.IsMatch(s, "\d") ->
            let afterCommaOrBracket =
                List.skipWhile (fun char -> char = "," || char = "]") rest

            Tip(int s), afterCommaOrBracket
        | s -> failwithf "unexpected char %A" s

    fst (inner str)


let sum (a: Node) (b: Node) = Node(a, b)

// this is absolutely terrible
let explode (n: Node) =
    let mutable hasExploded = false
    let mutable bubbleApplied = false

    let rec applyLeftBubble (node: Node) (value: int) =
        match node with
        | x when bubbleApplied = true -> x
        | Tip i ->
            bubbleApplied <- true
            Tip(i + value)
        | Node (Tip i, a) ->
            bubbleApplied <- true
            Node(Tip(i + value), a)
        | Node (a, b) -> Node(applyLeftBubble a value, applyLeftBubble b value)
        | x -> x

    let rec applyRightBubble (node: Node) (value: int) =
        match node with
        | x when bubbleApplied = true -> x
        | Tip i ->
            bubbleApplied <- true
            Tip(i + value)
        | Node (a, Tip i) ->
            bubbleApplied <- true
            Node(a, Tip(i + value))
        | Node (a, b) ->
            let right = applyRightBubble b value
            let left = applyRightBubble a value
            Node(left, right)
        | x -> x

    let applyLeft (node: Node) (value: int) =
        match node with
        | Tip i -> Tip(i + value)
        | Node (Tip i, Tip j) -> Node(Tip(i + value), Tip j)

    let applyRight (node: Node) (value: int) =
        match node with
        | Tip i -> Tip(i + value)
        | Node (Tip i, Tip j) -> Node(Tip i, Tip(j + value))

    let rec inner (n: int) (node: Node) =
        match n with
        | 3 when hasExploded = false ->

            match node with
            | Tip i -> Tip i, NoBubble
            | Node (Tip i, Tip j) -> Node(Tip i, Tip j), NoBubble
            | Node (Node (Tip i, Tip j), b) ->
                hasExploded <- true
                Node(Tip 0, applyLeft b j), LeftBubble i
            | Node (a, Node (Tip i, Tip j)) ->
                hasExploded <- true
                Node(applyRight a i, Tip 0), RightBubble j

        | n when n < 3 ->
            match node with
            | Node (left, right) ->
                let leftTraversed, bubbleFromLeft = inner (n + 1) left
                let rightTraversed, bubbleFromRight = inner (n + 1) right

                match (bubbleFromLeft, bubbleFromRight) with
                | (NoBubble, NoBubble) -> Node(leftTraversed, rightTraversed), NoBubble
                | (a, NoBubble) ->
                    match a with
                    | LeftBubble i -> Node(leftTraversed, rightTraversed), LeftBubble i
                    | RightBubble i ->
                        let rightMaybeBubbled = applyLeftBubble rightTraversed i

                        match bubbleApplied with
                        | true -> Node(leftTraversed, rightMaybeBubbled), NoBubble
                        | false -> Node(leftTraversed, rightMaybeBubbled), RightBubble i
                | (NoBubble, a) ->
                    match a with
                    | RightBubble i -> Node(leftTraversed, rightTraversed), RightBubble i
                    | LeftBubble i ->
                        let leftMaybeBubbled = applyRightBubble leftTraversed i

                        match bubbleApplied with
                        | true -> Node(leftMaybeBubbled, rightTraversed), NoBubble
                        | false -> Node(leftMaybeBubbled, rightTraversed), RightBubble i
                | s -> failwithf "bubbling fail %A" s
            | x -> x, NoBubble
        | _ -> node, NoBubble


    fst (inner 0 n), hasExploded

let rUp (x: int) =
    let f = float x
    Math.Ceiling(f / 2.) |> int

let rDown (x: int) =
    let f = float x
    Math.Floor(f / 2.) |> int

let split (n: Node) =
    let mutable hasSplat = false

    let rec inner (node: Node) =
        match node with
        | Tip i ->
            if i >= 10 && hasSplat = false then
                hasSplat <- true
                Node(Tip(rDown i), Tip(rUp i))
            else
                Tip i
        | Node (left, right) -> Node(inner left, inner right)

    inner n, hasSplat

let print (node: Node) =
    let rec innerPrint (acc: string list) (n: Node) =
        match n with
        | Tip i -> [ string i ]
        | Node (a, b) ->
            acc
            @ [ "[" ]
              @ (innerPrint [] a)
                @ [ "," ] @ (innerPrint [] b) @ [ "]" ]

    innerPrint [] node
    |> String.concat ""
    |> printfn "%A"

let rec reduce (n: Node) =
    //print n
    let exploded, hasExploded = explode n

    match hasExploded with
    | true -> reduce exploded
    | false ->
        let splat, hasSplat = split exploded
        if hasSplat then reduce splat else splat

let parsedInput = input |> List.map parse

let ans1 =
    parsedInput
    |> List.skip 1
    |> List.fold
        (fun state x ->
            let s = sum state x
            let newN = reduce s
            //print newN
            newN)
        (List.head parsedInput)

let rec magnitude (node: Node) =
    match node with
    | Tip i -> i
    | Node (a, b) -> 3 * magnitude a + 2 * magnitude b

printfn "%A" (ans1)
printfn "%A" (magnitude ans1)

let ans2 =
    parsedInput
    |> List.allPairs parsedInput
    |> List.map (fun (a, b) -> sum a b |> reduce |> magnitude)
    |> List.max

printfn "%A" (ans2)
