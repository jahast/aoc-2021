open System
open System.IO

type PacketTypeId = int
type PacketVersion = int
type LiteralValue = LiteralValue of int64
type Literal = PacketTypeId * PacketVersion * LiteralValue

type Packet =
    | Literal of Literal
    | Operator of PacketTypeId * PacketVersion * (Packet list)

let toChars (s: string) = s |> List.ofSeq |> List.map string

let hexMap =
    File.ReadAllLines("./hex.txt")
    |> Array.toList
    |> List.map
        (fun s ->
            s.Split("=", StringSplitOptions.TrimEntries)
            |> List.ofArray)
    |> List.map (fun xs -> (xs.Item(0), xs.Item(1)))
    |> Map

let hexToStringList (s: string) =
    s
    |> toChars
    |> List.map (fun x -> Map.find x hexMap)
    |> String.concat ""
    |> toChars


let toInt64 (xs: string list) =
    xs
    |> List.map int64
    |> List.rev
    |> List.mapi (fun i x -> x * pown (int64 2) i)
    |> List.sum

let toInt (xs: string list) =
    xs
    |> List.map int
    |> List.rev
    |> List.mapi (fun i x -> x * pown 2 i)
    |> List.sum

let input =
    File.ReadAllLines("./input.txt")
    |> List.ofArray
    |> List.head


let parseLiteral (xs: string list) =
    let rec parseInner xs acc =
        let cur, rest = xs |> List.splitAt 5

        match cur with
        | head :: tail when head = "1" -> parseInner rest (acc @ tail)
        | _ :: tail -> acc @ tail

    let res = parseInner xs []
    let toSkip = (List.length res) / 4 * 5
    let literalValue = res |> toInt64
    (literalValue, List.skip toSkip xs)


let rec parse (xs: string list) =

    let packetVersionBin, afterVersion = xs |> List.splitAt 3
    let packetTypeIdBin, content = afterVersion |> List.splitAt 3

    let packetVersion, packetTypeId =
        toInt packetVersionBin, toInt packetTypeIdBin

    match packetTypeId with
    | 4 ->
        let literalValue, rest = parseLiteral content

        let package =
            Literal(packetTypeId, packetVersion, LiteralValue(literalValue))

        (package, rest)
    | _ ->
        let lengthTypeId, afterLengthType = List.head content, List.tail content

        match lengthTypeId with
        | "0" ->
            let subPackageLengthBin, afterLength = List.splitAt 15 afterLengthType
            let subPackageLength = toInt subPackageLengthBin

            let subPackageContent, rest =
                List.splitAt subPackageLength afterLength

            let rec inner (acc: Packet list) xs =
                let left = List.length xs

                match left with
                | 0 -> acc
                | _ ->
                    let p, r = parse xs
                    inner (acc @ [ p ]) r

            let subPackages = inner [] subPackageContent

            let package =
                Operator(packetTypeId, packetVersion, subPackages)

            (package, rest)

        | _ ->
            let subPackageLengthBin, afterLength = List.splitAt 11 afterLengthType
            let subPackageLength = toInt subPackageLengthBin

            let rec inner n acc xs =
                match n with
                | 0 -> acc, xs
                | _ ->
                    let p, r = parse xs
                    inner (n - 1) (acc @ [ p ]) r

            let subPackages, rest = inner subPackageLength [] afterLength

            let package =
                Operator(packetTypeId, packetVersion, subPackages)

            (package, rest)

let rec sumVersionNumbers (packet: Packet) =
    let rec inner (packet: Packet) (acc: int) =
        match packet with
        | Literal (_, PacketVersion, _) -> acc + PacketVersion
        | Operator (_, PacketVersion, packets) ->
            let subPackageSum =
                packets |> List.map sumVersionNumbers |> List.sum

            acc + PacketVersion + subPackageSum

    inner packet 0

let ans1 = input |> hexToStringList |> parse |> fst

printfn "%A" (sumVersionNumbers ans1)

let rec calculateValue (p: Packet) : int64 =
    match p with
    | Literal (_, _, LiteralValue x) -> x
    | Operator (PacketTypeId, _, packets) ->
        match PacketTypeId with
        | 0 ->
            packets
            |> List.fold (fun state x -> state + calculateValue x) (int64 0)
        | 1 ->
            packets
            |> List.fold (fun state x -> state * calculateValue x) (int64 1)
        | 2 ->
            packets
            |> List.map (fun x -> calculateValue x)
            |> List.min
        | 3 ->
            packets
            |> List.map (fun x -> calculateValue x)
            |> List.max
        | 5 ->
            let first, second =
                calculateValue (packets.Item(0)), calculateValue (packets.Item(1))

            if first > second then 1 else 0
        | 6 ->
            let first, second =
                calculateValue (packets.Item(0)), calculateValue (packets.Item(1))

            if first < second then 1 else 0
        | 7 ->
            let first, second =
                calculateValue (packets.Item(0)), calculateValue (packets.Item(1))

            if first = second then 1 else 0
        | s -> failwithf "invalid packet type id %A" s

let ans2 = calculateValue ans1

printfn "%A" ans2
