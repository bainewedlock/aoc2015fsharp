#load "input.fsx"

open System

let alphabet = ['a'..'z'] |> List.except ['i';'o';'l'] |> List.indexed
let encodeMap = alphabet |> List.map (fun (a,b) -> (b,a)) |> Map 
let decodeMap = alphabet |> Map

let findSequence offset numbers =
  numbers
  |> List.scan (fun acc x -> 
    match acc with
    | (n, x') when x = x' + offset ->
      (n+1, x)
    | _ ->
      (1, x)) (0, 0)
    |> List.tail
    |> List.map fst

let hasStraightOfThree numbers =
  findSequence -1 numbers
  |> List.contains 3

let hasTwoPairs numbers =
  findSequence 0 numbers
  |> List.filter (fun n -> n = 2)
  |> List.length
  |> fun x -> x > 1

let valid numbers =
  hasStraightOfThree numbers
  && hasTwoPairs numbers

let isZ x = x = alphabet.Length - 1

let (|KillZs|_|) numbers =
  let zs = numbers |> List.takeWhile isZ
  let n = zs.Length
  if n > 0 then Some (n, List.skip n numbers)
  else None

let next = function
  | KillZs (z, x::rest) ->
    let zeros = List.replicate z 0
    List.append zeros (x+1::rest)
  | head::tail ->
    (head+1)::tail
  | bad ->
    failwithf "unexpected: '%A'" bad

let rec nextUntil counter f numbers =
  let test = next numbers
  if f test then test
  else 
    nextUntil (counter + 1) f test

let answer password =
  password
  |> Seq.toList
  |> List.rev
  |> List.map (fun x -> encodeMap.Item(x))
  |> nextUntil 0 valid
  |> List.map (fun x -> decodeMap.Item(x))
  |> List.rev
  |> fun chars -> String.Concat(chars)

#time
answer (Input.text "11")
#time

#time
answer (answer (Input.text "11"))
#time
