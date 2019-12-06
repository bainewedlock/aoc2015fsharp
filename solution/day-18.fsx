#load "input.fsx"

let parseInput input =
  let parseLine y line =
    line
    |> Seq.toList
    |> List.indexed
    |> List.choose (function
      | x, '#' -> Some (x, y)
      | _ -> None)
  Input.asList input
  |> List.mapi parseLine
  |> List.concat
  |> Set

let circleAround (x, y) = Set [
  for dx in [-1..1] do
  for dy in [-1..1] do
    match dx, dy with
    | 0, 0 -> ()
    | dx, dy -> yield (x+dx, y+dy)]

let inBounds size (x, y) =
  x >= 0 && x < size &&
  y >= 0 && y < size

let tick size state =
  let relevant = 
    state
    |> Set.toList
    //|> List.collect (fun cell ->
    //  circleAround cell |> Seq.filter (inBounds size) |> Seq.toList)
    |> List.collect (circleAround >> Seq.filter (inBounds size) >> Seq.toList)
    |> Set
  relevant
  |> Set.map (fun cell ->
    circleAround cell
    |> Set.intersect state
    |> Set.count
    |> fun c -> (cell, c))
  |> Set.fold (fun on (cell, n) ->
      match state.Contains(cell), n with 
      | true, 2 | true, 3 -> cell::on
      | false, 3 -> cell::on
      | _, _ -> on) []
  |> Set
let solve size steps input fIntercept =
  {1..steps}
  |> Seq.fold (fun state t ->
    printfn "tick %d" t
    state
    |> fIntercept
    |> tick size
    |> fIntercept)
      (parseInput input)
  |> Set.count
let sample = solve 6 4 "sample-18" id

#time
let answer = solve 100 100 "input-18"
#time
//Real: 00:02:48.796, CPU: 00:02:48.125, GC gen0: 63005, gen1: 40, gen2: 1
//val answer : int = 1061

let switchOnCorners size state =
  Set [
    for x in [0; size-1] do
    for y in [0; size-1] do
      yield (x, y)]
  |> Set.union state

let sample' = solve 6 5 "sample-18" (switchOnCorners 6)

#time
let answer' = solve 100 100 "input-18" (switchOnCorners 100)
#time
//Real: 00:02:51.998, CPU: 00:02:50.984, GC gen0: 62499, gen1: 43, gen2: 2
//val answer' : int = 1006
