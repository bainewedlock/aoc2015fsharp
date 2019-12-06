open System
open System.Text.RegularExpressions
open System.Collections.Generic

#load "input.fsx"

let example =
  "123 -> x
  456 -> y
  x AND y -> d
  x OR y -> e
  x LSHIFT 2 -> f
  y RSHIFT 2 -> g
  NOT x -> h
  NOT y -> i".Split('\n')

let ParseLine (line : string) =
  let parts = line.Split([|" -> "|], StringSplitOptions.None)
  (parts.[1], parts.[0].Trim())

let MatchTemplate (template : string) (input : string) =
  let pattern = sprintf "^%s$" (template
    .Replace("$d", "(\d+)")
    .Replace("$w", "(\w+)"))
  Regex.Match(input, pattern)

let (|OneMatch|_|) (template : string) (input : string) =
  let m = MatchTemplate template input
  if m.Success then Some m.Groups.[1].Value
  else None

let (|TwoMatches|_|) (template : string) (input : string) =
  let m = MatchTemplate template input
  if m.Success then Some (m.Groups.[1].Value, m.Groups.[2].Value)
  else None

// Source: https://wj32.org/wp/2016/01/13/f-code-memoize-a-recursive-function/
let memoize f =
  let mem = Dictionary<'a, 'b>();
  let rec g key = h g key
  and h r key =
    match mem.TryGetValue(key) with
    | (true, value) -> value
    | _ ->
      let value = f g key
      mem.Add(key, value)
      value
  g

#time
let answer =
  let instructions = Input.asLines "7" |> Seq.map ParseLine |> dict
  let Eval recur wire =
    match wire with
    | OneMatch "$d" a -> int a
    | wire ->
      match instructions.Item(wire) with
      | OneMatch "$w" a -> recur a
      | TwoMatches "$w AND $w" (a, b) -> (recur a) &&& (recur b)
      | TwoMatches "$w OR $w" (a, b) -> (recur a) ||| (recur b)
      | TwoMatches "$w LSHIFT $d" (a, b) -> (recur a) <<< int b
      | TwoMatches "$w RSHIFT $d" (a, b) -> (recur a) >>> int b
      | OneMatch "NOT $w" a -> int (~~~ (uint16 (recur a)))
      | instruction -> failwithf "unexpected instruction: '%s'" instruction
  (memoize Eval) "a"
#time


#time
let answer' =
  let Rewire wire newValue (key, value) =
    if key = wire then
      (key, newValue)
    else 
      (key, value)
  let instructions =
    Input.asLines "7"
    |> Seq.map ParseLine
    |> Seq.map (Rewire "b" (string answer))
    |> dict
  let Eval recur wire =
    match wire with
    | OneMatch "$d" a -> int a
    | wire ->
      match instructions.Item(wire) with
      | OneMatch "$w" a -> recur a
      | TwoMatches "$w AND $w" (a, b) -> (recur a) &&& (recur b)
      | TwoMatches "$w OR $w" (a, b) -> (recur a) ||| (recur b)
      | TwoMatches "$w LSHIFT $d" (a, b) -> (recur a) <<< int b
      | TwoMatches "$w RSHIFT $d" (a, b) -> (recur a) >>> int b
      | OneMatch "NOT $w" a -> int (~~~ (uint16 (recur a)))
      | instruction -> failwithf "unexpected instruction: '%s'" instruction
  (memoize Eval) "a"
#time
