open System

#load "input.fsx"

let intRoot x = float x |> sqrt |> int

let isPrime x =
  match x with
  | 1 -> false
  | _ ->
    {2..intRoot x}
    |> Seq.forall (fun t -> x % t > 0)

let primesUpto x = 
  [2..x]
  |> List.filter isPrime

let divisors primes x =
  let max = intRoot x
  primes
  |> List.takeWhile ((>=) max)
  |> List.collect (fun p -> [p..p..max])
  |> List.collect (fun i ->
    match x/i with
    | _ when x % i > 0 -> []
    | n when n = i -> [n]
    | n -> [i; n])
  |> List.append [1; x]
  |> List.distinct

let inifiteElves x x' = true
let limitedElves x x' = x / x' <= 50
let solve fElf factor n =
  let primes = primesUpto <| intRoot n
  let house x =
    divisors primes x
    |> List.filter (fElf x)
    |> List.sum
    |> fun s -> s * factor
  let rec loop x =
    match house x with
    | h when h >= n ->
      (x, h)
    | h ->
      if x % 100000 = 0 then
        printfn "%d->%d is too low @ %A"
          x h DateTimeOffset.Now
      loop <| x+1
  loop 1

let input = Input.asText "input-20" |> int
 
#time
let answer = solve inifiteElves 10 34000000
#time
//val it : int * int = (786240, 34137600)

#time
let answer' = solve limitedElves 11 34000000
#time
//Real: 00:00:56.527, CPU: 00:00:55.781, GC gen0: 51408, gen1: 8, gen2: 1
//val solve : fElf:'a -> factor:int -> n:int -> start:int -> int * int
//val answer' : int * int = (831600, 35780206)
