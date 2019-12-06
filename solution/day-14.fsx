#load "input.fsx"

open System.Text.RegularExpressions
open System.Linq

type State = { 
  Reindeers : int list list
  Mileage : int list
  Scores : int list
} with static member create xs = {
        Reindeers = xs
        Mileage = List.replicate xs.Length 0 
        Scores = List.replicate xs.Length 0 } 

module Scoring =
  type Func = int list -> int list -> int list

  let distanceBased scores mileage =
    mileage

  let awardLeader scores mileage =
    let max = mileage |> List.max
    mileage
    |> List.zip scores
    |> List.map (fun (s, m) -> if m = max then s+1 else s)
    |> List.rev

let parseReindeer duration line =
  let mx = Regex.Matches(line, @"\d+").Cast<Match>() |> Seq.toList
  match mx |> List.map (fun x -> int x.Value) with
  | [fly;sec;sleep] ->
    seq { while true do
          yield! (Seq.replicate sec fly)
          yield! (Seq.replicate sleep 0) }
    |> Seq.take duration
    |> Seq.toList
  | err -> failwithf "unexpected: '%A'" err

let headReindeers reindeers =
  reindeers
  |> List.rev
  |> List.fold (fun (scores, reindeers') r ->
    ((List.head r)::scores, (List.tail r)::reindeers'))
      (List.empty, List.empty)

let tickState (fScore : Scoring.Func) { Reindeers = rs; Mileage = ms; Scores = ss; } =
  let (speeds, rs') = headReindeers rs
  let ms' =
    List.zip ms speeds
    |> List.map (fun (a, b) -> a+b)
  let ss' = fScore ss ms' |> List.rev
  Some (ss', { Reindeers = rs'; Mileage = ms'; Scores = ss' })
  
let race file duration fScoring =
  Input.asLines file
  |> Array.toList
  |> List.map (parseReindeer duration)
  |> State.create
  |> Seq.unfold (tickState fScoring)
  |> Seq.skip (duration - 1)
  |> Seq.head
  |> List.max

let sample = race "14sample" 1000 Scoring.distanceBased
let answer = race "14" 2503 Scoring.distanceBased
let sample' = race "14sample" 1000 Scoring.awardLeader
let answer' = race "14" 2503 Scoring.awardLeader

