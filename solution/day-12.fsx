#load "input.fsx"

open System.Linq
open System.Text.RegularExpressions

let answer = 
  let input = (Input.text "12")
  Regex.Matches(input, @"(?<!"")-?\d+(?!"")").Cast<Match>()
  |> Seq.sumBy (fun m -> int m.Value)

#r @"C:/users/wboec/.nuget/packages/fsharp.data/3.3.2/lib/net45/FSharp.Data.dll"
open FSharp.Data

let hasRed = function
  | (_, JsonValue.String x) when x = "red" -> true
  | _ -> false
let rec extract node =
  match node with
  | JsonValue.Array xs ->
    xs |> Array.toList |> List.collect extract
  | JsonValue.Record xs ->
    if xs |> Array.exists hasRed then []
    else xs |> Array.toList |> List.collect (fun (_, v) -> extract v)
  | JsonValue.Number x -> [x]
  | _ -> []

let answer' =
  JsonValue.Parse(Input.text "12")
  |> extract
  |> List.sum

