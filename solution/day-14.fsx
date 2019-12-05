#load "input.fsx"

open System.Text.RegularExpressions
open System.Linq

let parse line =
  let mx = Regex.Matches(line, @"\d+").Cast<Match>() |> Seq.toList
  match mx |> List.map (fun x -> int x.Value) with
  | [fly;sec;sleep] ->
    seq { while true do
          yield! (Seq.replicate sec fly)
          yield! (Seq.replicate sleep 0) }
  
let answer =
  Input.lines "14"
  |> Array.toList
  |> List.map parse
  |> List.map (Seq.take 2503 >> Seq.sum)
  |> List.max

