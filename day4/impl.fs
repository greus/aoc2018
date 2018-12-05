module day4

open System
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

// "C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\Common7\IDE\CommonExtensions\Microsoft\FSharp\fsi.exe"

let readLines filePath = System.IO.File.ReadLines(filePath)

type BeginsShift = { Time : DateTime; GuardId : int }
type FallsAsleep = { Time : DateTime; }
type WakesUp = { Time : DateTime; }

type Event =
    | BeginsShift of BeginsShift
    | FallsAsleep of FallsAsleep
    | WakesUp of WakesUp

    member self.GetTime =
        match self with
        | BeginsShift b -> b.Time
        | FallsAsleep f -> f.Time
        | WakesUp w -> w.Time


let mapEvents s : Event =
    match s with
    | Regex @"\[(.*)\] Guard #(.*) begins shift" [ dt; guard ] -> BeginsShift { Time = DateTime.Parse(dt); GuardId = int guard }
    | Regex @"\[(.*)\] falls asleep" [ dt ] -> FallsAsleep { Time = DateTime.Parse(dt) }
    | Regex @"\[(.*)\] wakes up" [ dt ] -> WakesUp { Time = DateTime.Parse(dt) }
    | _ -> failwith "Invalid event"

let setAsleep acc fallAsleep =
    // printfn "Falls asleep"
    match acc with
    | (g, _, minutes) :: tail -> (g, fallAsleep, minutes) :: tail
    | _ -> failwith "should not happen"

let countAsleep(acc:List<int * DateTime * List<int>>, wokeUp:DateTime) =
    // printfn "Count asleep"
    match acc with
    | (g : int, fallAsleep : DateTime, minutes : List<int>) :: tail ->
        (g, wokeUp, [fallAsleep.Minute .. wokeUp.Minute-1] @ minutes) :: tail
    | _ -> failwith "should not happen"

let foo(acc:List<int * DateTime * List<int>>, e) =
    // printfn "%A" acc
    match e with
    | BeginsShift { Time=t; GuardId=g } -> ((g, t, []) :: acc)
    | FallsAsleep { Time=t } -> setAsleep acc t
    | WakesUp { Time=t } -> countAsleep(acc, t)
    | _ -> acc

let groupEvents events =
    events
    |> List.fold (fun acc e -> foo(acc, e)) []

let sum (guard, events) =
    guard, List.fold (fun acc (_, _, e) -> acc @ e) [] events

let result g (m, _) =
    let checksum = g * m
    (g, m, checksum)

let findSleepiestMinute (g, _, e) =
    e
    |> List.groupBy (fun x -> x)
    |> List.sortByDescending (fun (a, b) -> List.length b)
    |> List.head
    |> result g

let result2 g l =
    match l with
    | [] -> (g, 0, 0)
    | (m, x)::_ -> (g, m, List.length x)

let findSleepiestMinute2 (g, _, e) =
    e
    |> List.groupBy (fun x -> x)
    |> List.sortByDescending (fun (_, b) -> List.length b)
    |> result2 g

let test () =
    ["[1518-11-01 00:00] Guard #10 begins shift";
    "[1518-11-01 00:05] falls asleep";
    "[1518-11-01 00:25] wakes up";
    "[1518-11-01 00:30] falls asleep";
    "[1518-11-01 00:55] wakes up";
    "[1518-11-01 23:58] Guard #99 begins shift";
    "[1518-11-02 00:40] falls asleep";
    "[1518-11-02 00:50] wakes up";
    "[1518-11-03 00:05] Guard #10 begins shift";
    "[1518-11-03 00:24] falls asleep";
    "[1518-11-03 00:29] wakes up";
    "[1518-11-04 00:02] Guard #99 begins shift";
    "[1518-11-04 00:36] falls asleep";
    "[1518-11-04 00:46] wakes up";
    "[1518-11-05 00:03] Guard #99 begins shift";
    "[1518-11-05 00:45] falls asleep";
    "[1518-11-05 00:55] wakes up"]
    |> Seq.map mapEvents
    |> Seq.sortBy (fun e -> e.GetTime)
    |> Seq.toList
    |> groupEvents
    |> List.groupBy (fun (g, _, _) -> g)
    |> List.map sum
    |> List.map (fun (g, e) -> (g, List.length e, e))
    |> List.sortByDescending (fun (_, l, _) -> l)
    |> List.head
    |> findSleepiestMinute
    |> printfn "%A"

let test2 () =
    ["[1518-11-01 00:00] Guard #10 begins shift";
    "[1518-11-01 00:05] falls asleep";
    "[1518-11-01 00:25] wakes up";
    "[1518-11-01 00:30] falls asleep";
    "[1518-11-01 00:55] wakes up";
    "[1518-11-01 23:58] Guard #99 begins shift";
    "[1518-11-02 00:40] falls asleep";
    "[1518-11-02 00:50] wakes up";
    "[1518-11-03 00:05] Guard #10 begins shift";
    "[1518-11-03 00:24] falls asleep";
    "[1518-11-03 00:29] wakes up";
    "[1518-11-04 00:02] Guard #99 begins shift";
    "[1518-11-04 00:36] falls asleep";
    "[1518-11-04 00:46] wakes up";
    "[1518-11-05 00:03] Guard #99 begins shift";
    "[1518-11-05 00:45] falls asleep";
    "[1518-11-05 00:55] wakes up"]
    |> Seq.map mapEvents
    |> Seq.sortBy (fun e -> e.GetTime)
    |> Seq.toList
    |> groupEvents
    |> List.groupBy (fun (g, _, _) -> g)
    |> List.map sum
    |> List.map (fun (g, e) -> (g, List.length e, e))
    |> List.map findSleepiestMinute2
    // |> List.sortByDescending (fun (g, l) -> l)
    // |> List.head
    // |> printfn "%A"


let p1 () =
    readLines "input"
    |> Seq.map mapEvents
    |> Seq.sortBy (fun e -> e.GetTime)
    |> Seq.toList
    |> groupEvents
    |> List.groupBy (fun (g, _, _) -> g)
    |> List.map sum
    |> List.map (fun (g, e) -> (g, List.length e, e))
    |> List.sortByDescending (fun (_, l, _) -> l)
    |> List.head
    |> findSleepiestMinute
    |> printfn "%A"

let p2answer (g, m, _) =
    g * m

let p2 () =
    readLines "input"
    |> Seq.map mapEvents
    |> Seq.sortBy (fun e -> e.GetTime)
    |> Seq.toList
    |> groupEvents
    |> List.groupBy (fun (g, _, _) -> g)
    |> List.map sum
    |> List.map (fun (g, e) -> (g, List.length e, e))
    |> List.map findSleepiestMinute2
    |> List.sortByDescending (fun (g, m, l) -> l)
    |> List.head
    |> p2answer
