#load "input.fsx"

module Compositions =
  let rec private next n unused acc = seq {
    match unused with
    | [] -> yield acc
    | [_] -> yield n::acc
    | _::tail ->
      for i in { 0..n } do
        yield! next (n-i) tail <| i::acc
  }
  let generate n t =
    next t (List.replicate n 0) []
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

type Ingredient =

  { properties: int list; calories: int }

  static member (*) (x, n) =
    let multiply = (fun v -> v * n)
    {
      properties = x.properties |> List.map multiply
      calories = x.calories |> multiply
    }

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
  |> List.map fst
  |> List.max

let sample = solve "sample-15" (fun _ -> true)
let answer = solve "input-15" (fun _ -> true)
let sample' = solve "sample-15" (fun c -> c = 500)
let answer' = solve "input-15" (fun c -> c = 500)
