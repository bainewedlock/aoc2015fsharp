#load "input.fsx"

module Compositions =
  let rec private next (t : int) (acc : int list) =
    let output f = Some (f, f)
    match acc with
    | [] -> None
    | head::tail ->
      let s = List.sum acc
      if s < t
      then output <| (head+1)::tail
      else
        match next t tail with
        | Some (_, rest) ->
          output <| 0::rest
        | None -> None

  let generate n t =
    List.replicate n 0
    |> Seq.unfold (next t)
    |> Seq.toList  

module Matrix = 
  let rec transpose matrix = 
    match matrix with
    | row::rows ->
      match row with
      | col::cols ->
        let first = List.map List.head matrix
        let rest = transpose (List.map List.tail matrix) 
        first :: rest
      | _ -> []
    | _ -> [] 

type Ingredient = { properties: int list; calories: int }
module Ingredient =
  let parse (line : string) =
// Example:
// "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8" 
    let tokens = line.Replace(",", "").Split ' '
    { 
      properties = [
        int tokens.[2];
        int tokens.[4];
        int tokens.[6];
        int tokens.[8]
      ]
      calories = int tokens.[10]
    }

  let score (ingredients : Ingredient list) =
    let value = 
      ingredients
      |> List.map (fun i -> i.properties)
      |> Matrix.transpose
      |> List.map (List.sum >> max 0)
      |> List.reduce (*)
    let calories =
      ingredients
      |> List.sumBy (fun i -> i.calories)
    (value, calories)
  let scale (x : Ingredient, factor : int) =
    let multiply = (fun v -> v * factor)
    {
      properties = x.properties |> List.map multiply
      calories = multiply x.calories
    }

let solve input caloriesFilter =
  let ingredients = Input.asList input |> List.map Ingredient.parse
  Compositions.generate <| ingredients.Length <| 100
  |> List.map (List.zip ingredients >> List.map Ingredient.scale)
  |> List.map Ingredient.score
  |> List.filter (fun (_, c) -> caloriesFilter c)
  |> List.maxBy (fun (s, _) -> s)

let sample = solve "sample-15" (fun _ -> true)
#time
let answer = solve "input-15" (fun _ -> true)
#time
//Real: 00:01:06.984, CPU: 00:01:06.390, GC gen0: 4736, gen1: 383, gen2: 7
//val answer : int * int = (18965440, 554)

let sample' = solve "sample-15" (fun c -> c = 500)
#time
let answer' = solve "input-15" (fun c -> c = 500)
#time
//Real: 00:00:42.806, CPU: 00:00:42.984, GC gen0: 4753, gen1: 474, gen2: 3
//val answer' : int * int = (15862900, 500)

