module Day7

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines(filePath)

let mapDependencies s =
    match s with
    | Regex @"Step (.) must be finished before step (.) can begin." [ step; dependency ] -> (step, dependency)
    | _ -> failwith "Invalid input"

let foo list =
    let dependencies = List.map (fun (_, d) -> d) list
    let firstInLine =
        list
        |> List.filter (fun (s, _) -> not (List.contains s dependencies))
        |> List.sortBy (fun (s, _) -> s)

    let first =
        firstInLine
        |> List.distinct
        |> List.head

    let x, y = first
    printf "%s" x

    list
    |> List.filter (fun (s, _) -> s <> x)


let rec stepsOrder list =
    match list with
    | [] -> printfn ""
    | l -> l |> foo |> stepsOrder

let not b = not b
let addStepsFrom dependencies =
    let missing =
        List.fold (fun acc (s, d) -> [s] @ [d] @ acc) [] dependencies
        |> List.distinct
        |> List.filter (fun a -> dependencies |> List.exists (fun (b, _) -> a = b) |> not)

    let add = List.map (fun s -> (s, "")) missing
    add @ dependencies

let part1 inputLines =
    inputLines
    |> Seq.toList
    |> List.map mapDependencies
    |> addStepsFrom
    |> stepsOrder


let test () =
    ["Step C must be finished before step A can begin.";
    "Step C must be finished before step F can begin.";
    "Step A must be finished before step B can begin.";
    "Step A must be finished before step D can begin.";
    "Step B must be finished before step E can begin.";
    "Step D must be finished before step E can begin.";
    "Step F must be finished before step E can begin.";]
    |> part1

let p1 () =
    readLines "input"
    |> part1
