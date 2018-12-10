module day5

open System

let readText filePath = System.IO.File.ReadAllText(filePath)

// https://stackoverflow.com/a/20308915
let stripchars chars str =
  Seq.fold
    (fun (str: string) chr ->
      str.Replace(chr |> Char.ToUpper |> string, "").Replace(chr |> Char.ToLower |> string, ""))
    str chars

let didReact c1 c2 =
    Char.ToUpper c1 = Char.ToUpper c2 && not(c1 = c2)

let rec anyReaction chars =
    match chars with
    | [] -> (false, [])
    | char::rest ->
        match rest with
        | [] -> (false, [])
        | next::rest2 -> if didReact char next then (true, [char;next]) else anyReaction rest

let rec react chars =
    match anyReaction chars with
    | false, _ -> chars
    | true, l ->
        (chars
        |> List.toArray
        |> System.String
        ).Replace (l |> List.toArray |> System.String, "")
        |> Seq.toList
        |> react

let test () =
    "dabAcCaCBAcCcaDA"
    |> Seq.toList
    |> react
    |> List.toArray |> System.String


let p1 () =
    readText "input"
    |> stripchars ['\n']
    |> Seq.toList
    |> react
    |> List.length

let p2 () =
    let str = readText "input" |> stripchars ['\n']
    let az = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y']

    az
    |> List.map (fun letter -> (letter, str |> stripchars [letter] |> Seq.toList |> react |> List.length))
    |> List.minBy (fun (_, length) -> length)
