open System

#load "input.fsx"

let intRoot x = float x |> sqrt |> int

let divisors x =
  let max = intRoot x
  [ 1..max ]
  |> List.filter  (fun x' -> x % x' = 0)
  |> List.collect (fun x' -> [x'; x/x'])

let solve fElf factor n =
  let house x =
    divisors x
    |> List.filter (fElf x)
    |> List.sum
    |> fun s -> s * factor
  let rec loop x =
    match house x with
    | h when h >= n -> (x, h)
    | h ->
      if x % 100000 = 0 then
        printfn "%d->%d is too low @ %A"
          x h DateTimeOffset.Now
      loop <| x+1
  loop 1

let input = Input.asText "input-20" |> int
 
#time
let inifiteElves x x' = true
let answer = solve inifiteElves 10 input
#time
//Real: 00:00:12.423, CPU: 00:00:12.375, GC gen0: 10335, gen1: 1, gen2: 0
//val answer : int * int = (786240, 34137600)

#time
let limitedElves x x' = x / x' <= 50
let answer' = solve limitedElves 11 input
#time
//Real: 00:00:11.902, CPU: 00:00:11.875, GC gen0: 11065, gen1: 1, gen2: 0
//val answer' : int * int = (831600, 35780206)
