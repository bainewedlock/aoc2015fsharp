#load "input.fsx"

open System.Text.RegularExpressions

let parseLine (line : string) =
  let m = Regex.Match(line, @"^(\w+) to (\w+) = (\d+)$")
  let a = m.Groups.[1].Value
  let b = m.Groups.[2].Value
  let distance = int m.Groups.[3].Value
  [| (a, (distance, b)); (b, (distance, a)) |]

let optimizeGrouping (key, grouping) =
  let sorted =
    grouping
    |> Seq.map snd
    |> Seq.sortBy fst
  (key, sorted)

let parseRoutes =
  Array.map parseLine >>
  Array.concat >>
  Array.groupBy fst >>
  Array.map optimizeGrouping >>
  dict

let analyzeFile factor file =
  let routes = Input.asArray file |> parseRoutes
  let rec travel miles locations currentLocation =
    let validRoute (_, loc) = Set.contains loc locations
    if Set.isEmpty locations then miles
    else 
      let (distance, next_stop) =
        routes.Item(currentLocation)
        |> Seq.filter validRoute
        |> Seq.minBy (fun (distance, _) -> factor * distance)
      let remaining_locations =
        locations
        |> Set.remove currentLocation
        |> Set.remove next_stop
      travel (distance + miles) remaining_locations next_stop 
  let locations = routes.Keys |> Set.ofSeq
  locations
  |> Seq.map (travel 0 locations)
  |> Seq.minBy (fun x -> factor * x)

let sample = analyzeFile +1 "sample-9"
let answer = analyzeFile +1 "input-9"

let sample' = analyzeFile -1 "sample-9"
let answer' = analyzeFile -1 "input-9"
