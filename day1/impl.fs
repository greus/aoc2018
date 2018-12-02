module day1

let readLines filePath = System.IO.File.ReadLines(filePath)

let changeFreq (initial, change : string) = initial + (change |> int64)

let run initial =
    readLines "input1.txt"
    |> Seq.fold (fun acc elem -> changeFreq(acc, elem)) initial
    |> printfn "%d"

let twice initial =
    let changes = readLines "input1.txt" |> Seq.toList
    let mutable freq = initial
    let mutable history = [initial]
    let mutable firstTwice = None

    while firstTwice.IsNone do
        changes
        |> List.iter (fun change ->
            freq <- changeFreq(freq, change)

            match List.exists ((=) freq) history with
            | true when firstTwice.IsNone -> firstTwice <- Some freq
            | _ -> history <- freq::history
        )

    printfn "First frequency it reaches twice: %d" firstTwice.Value
