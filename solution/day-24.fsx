open System
#load "input.fsx"

type Container =
  {
    packages : int list
    capacity : int
  }
  with
    static member create c = { packages = []; capacity = c }
    static member (+) (c: Container, p: int) =
      {
        packages = p::c.packages
        capacity = c.capacity - p
      }

let calcQE (ps: int list) =
  ps |>
  List.map bigint
  |>List.reduce (*)

type Score =
  Score of int * bigint
  with
    static member calc (cs: Container list) = 
      cs
      |> List.map (fun c ->
          ( c.packages.Length,
            calcQE c.packages))
      |> List.minBy snd

let rec solve2 max (acc: Container list) (cs: Container list) (ps: int Set) = seq {
  match cs with
  | []      ->
    yield Score.calc acc
  | c::rest when c.capacity = 0 ->
    let max' = (ps |> Set.count) - rest.Length + 1
    yield! solve2 max' (c::acc) rest ps
  | c::_ when c.packages.Length >= max -> () // pruning
  | c::rest ->
    let fitting = ps |> Set.filter ((>=) c.capacity)
    for p in fitting do
      yield! solve2 max acc ((c+p)::rest) (ps |> Set.remove p |> Set)
  }

let rec solve max n ps =
  printfn "trying with compartment size = %d" max
  let capacity = (List.sum ps) / n
  let solutions = 
    solve2
      max
      List<Container>.Empty
      (List.replicate n (Container.create capacity))
      (Set ps)
  match Seq.isEmpty solutions with
  | true -> solve (max+1) n ps
  | _    -> solutions

let printBest sx =
  let mutable best = None
  sx
  |> Seq.iter (fun s ->
    if best.IsNone || s < best.Value then
      best <- Some s
      printfn "--------------------"
      printfn "%A" DateTimeOffset.Now
      printfn "%A" best) 

#time
let answer =
  Input.asList "input-24" |> List.map int
  |> solve 6 3
  |> Seq.head
  //|> printBest // takes forever, Seq.head worked
#time
//Real: 00:01:44.410, CPU: 00:01:44.109, GC gen0: 56503, gen1: 11, gen2: 1
//val answer : Score = { value = (6, 10439961859) }

#time
let answer' =
  Input.asList "input-24" |> List.map int
  |> solve 1 4
  |> Seq.head
  //|> printBest // takes forever, Seq.head worked
#time
//Real: 00:00:08.006, CPU: 00:00:07.875, GC gen0: 4211, gen1: 0, gen2: 0
//val answer' : Score = { value = (5, 72050269) }

