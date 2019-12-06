#load "input.fsx"
open System

let alphabet = ['a'..'z'] |> List.except ['i';'o';'l'] |> List.indexed
let encodeMap = alphabet |> List.map (fun (a,b) -> (b,a)) |> Map 
let decodeMap = alphabet |> Map

let hasStraightOfThree =
  List.windowed 3 >>
  List.exists (fun [a;b;c] ->
    a = b + 1 && b = c + 1)

let hasTwoPairs =
  List.windowed 2 >>
  List.filter (fun [a;b] -> 
    a = b) >>
  List.distinct >>
  (fun x -> x.Length >= 2)

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

let answer password =
  password
  |> Seq.toList
  |> List.rev
  |> List.map (fun x -> encodeMap.Item(x))
  |> Seq.unfold (fun numbers ->
    let candidate = next numbers
    Some (candidate, candidate))
  |> Seq.find valid
  |> List.map (fun x -> decodeMap.Item(x))
  |> List.rev
  |> fun chars -> String.Concat(chars)

#time
answer (Input.asText "input-11")
#time

#time
answer (answer (Input.asText "input-11"))
#time
