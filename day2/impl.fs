module day2

let readLines filePath = System.IO.File.ReadLines(filePath)

let hasPairAndTriple chars =
    let mutable counts = Map.empty
    chars
    |> Seq.iter (fun c ->
        match counts.TryFind(c) with
        | Some count -> counts <- Map.remove c counts;counts <- counts.Add(c, count+1)
        | _ -> counts <- counts.Add(c, 1)
    )

    let pairs = counts |> Map.filter (fun _ v -> v = 2)
    let triple = counts |> Map.filter (fun _ v -> v = 3)
    (pairs.Count > 0, triple.Count > 0)

let test () =
    "bababc"
    |> Seq.toList
    |> hasPairAndTriple
    |> printfn "%A"

let sum acc t =
    let ap, at = acc
    match t with
    | true, true -> (ap+1, at+1)
    | true, false -> (ap+1, at)
    | false, true -> (ap, at+1)
    | _, _ -> acc

let p1 () =
    readLines "input"
    |> Seq.map (fun l -> l |> Seq.toList |> hasPairAndTriple)
    |> Seq.fold sum (0,0)
    |> fun (pairs, triples) -> pairs * triples
    |> printfn "checksum: %d"
