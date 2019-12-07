#load "input.fsx"

open System
open System.Text.RegularExpressions
open System.Linq

let pattern = @"^(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.$"

let (|Int|) (s : string) = int s
let (|Person|) (s : string) = s.Substring(0, 1)

let rules = 
  Input.asArray "input-13"
  |> Array.toList
  |> List.map (fun l ->
    let m = Regex.Match(l, pattern)
    m.Groups.Cast<Group>()
    |> Seq.toList
    |> List.tail
    |> List.map (fun g -> g.Value))
  |> List.collect (function 
  | [Person x; "gain"; Int n; Person y] -> [(x+y, + n); (y+x, +n)]
  | [Person x; "lose"; Int n; Person y] -> [(x+y, - n); (y+x, -n)])

let people =
  rules
  |> Seq.toList
  |> List.map (fun (key, n) -> key.[0])
  |> List.distinct

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect <| distribute e <| permute xs

let circlePermute (people : char list) =
  match people with
  | fix::rest ->
    permute rest
    |> List.map (fun xs -> String.Concat(fix::xs@[fix]))
  | e -> failwithf "error %A" e

let eval (setting : string) =
  rules
  |> List.choose (fun (key, n) ->
    if setting.Contains(key) then Some n else None)
  |> List.sum

let answer =
  circlePermute people
  |> List.map eval
  |> List.max

let answer' =
  circlePermute <| 'Y'::people
  |> List.map eval
  |> List.max
