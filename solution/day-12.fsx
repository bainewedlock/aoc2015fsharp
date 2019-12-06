#load "input.fsx"

open System.Linq
open System.Text.RegularExpressions

let answer = 
  let input = (Input.asText "12")
  Regex.Matches(input, @"(?<!"")-?\d+(?!"")").Cast<Match>()
  |> Seq.sumBy (fun m -> int m.Value)

#r @"C:/users/wboec/.nuget/packages/fsharp.data/3.3.2/lib/net45/FSharp.Data.dll"
open FSharp.Data

let hasStringValue v = function
  | (_, JsonValue.String x) when x = v -> true
  | _ -> false

let rec extract node =
  match node with
  | JsonValue.Array xs -> xs |> Array.sumBy extract
  | JsonValue.Record xs ->
      if xs |> Array.exists (hasStringValue "red") then 0
      else xs |> Array.sumBy (fun (_, v) -> extract v)
  | JsonValue.Number x -> int x
  | _ -> 0
let answer' =
  JsonValue.Parse(Input.asText "12")
  |> extract
