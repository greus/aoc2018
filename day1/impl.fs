module day1

let readLines filePath = System.IO.File.ReadLines(filePath)

let changeFreq (initial, change : string) =
    let modifer = change.[0]
    let value = change.[1..change.Length-1] |> int
    match modifer with
    | '+' -> initial + value
    | _ -> initial - value

let run initial =
    readLines "input1.txt"
    |> Seq.fold (fun acc elem -> changeFreq(acc, elem)) initial
    |> printfn "%d"
