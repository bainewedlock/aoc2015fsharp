open System
open System.Text.RegularExpressions

#load "input.fsx"

let goal = "e"

type Rule =
  { left : string; right : string }
  with static member reverse rule =
        { left = rule.right; right = rule.left}

let (|Rule|_|) s =
  match Regex.Match(s, @"(\w+) => (\w+)") with
  | m when m.Success ->
    Some {
        left = m.Groups.[1].Value;
        right = m.Groups.[2].Value }
  | _ -> None

module Rule =
  let private replaceSubstring (s : string) i l (r : string) =
    s.Substring(0, i) + r + s.Substring(i + l)

  let rec apply (i : int) (molecule : string) rule  = [
    match molecule.IndexOf(rule.left, i) with
    | -1 -> ()
    | i' ->
      let n = rule.left.Length
      yield replaceSubstring molecule i' n rule.right
      yield! apply (i' + 1) molecule rule 
  ]

  let rec applyAll (rules : Rule list) molecule = Set [
    match rules with
    | [] -> ()
    | hd::tail ->
      yield! apply 0 molecule hd
      yield! applyAll tail molecule
  ]

  let parseLines lines = 
    lines
    |> List.filter ((<>) "")
    |> List.fold (fun (rules, m) -> function
      | Rule r -> (r::rules, m)
      | molecule -> (rules, Some molecule))
        ([], None)
    |> function
    | rules, Some molecule -> (rules, molecule)
    | _, None -> failwithf "Could not find molecule in input"

let solve file = 
  let (rules, molecule) = Rule.parseLines (Input.asList file)
  Rule.applyAll rules molecule
  |> Set.count

let sample = solve "sample-19"
let answer = solve "input-19"

let simplify input =
  let alphabet =
    [|'a'..'z'|]
    |> Array.append [|'A'..'Z'|]
    |> Array.append [|'0'..'9'|]
    |> Array.except [| char goal |]
  Rule.parseLines <| Input.asList input
  |> fst
  |> List.map (fun r -> r.left)
  |> List.distinct
  |> List.mapi (fun i x ->
    { left = x; right = string alphabet.[i] })
  |> List.filter (fun r ->
    r.left <> goal)
  |> List.fold (fun (acc : string) r ->
    acc.Replace(r.left, r.right)) (Input.asText input)
  |> fun text ->
    text.Split([|"\r\n"|], StringSplitOptions.None) 
  |> Array.toList

let rec solutions steps history rules (m : string) = seq {
  match m with
  | m when m = goal -> yield steps
  | m when Set.contains m history -> printfn "SKIPPING %s" m
  | m ->
    let ms' =
      rules
      |> List.collect (Rule.apply 0 m)
      |> Set
      |> Set.filter (fun m' -> m'.Length <= m.Length)
    let history' = history.Add m
    let steps' = steps + 1
    for m' in ms' do
      yield! solutions steps' history' rules m'
  }

let solve' input = 
  let (rules, molecule) = Rule.parseLines input
  let rules' = rules |> List.map Rule.reverse
  let result =
    solutions 0 Set.empty rules' molecule
    |> Seq.fold (fun min x ->
      if x < min || min = -1 then
        printfn "found %d steps @ %A, still running..." x DateTimeOffset.Now
        x
      else
        min) -1
  printfn "done"
  result
  
let sample' = solve' (Input.asList "sample-19")

#time
let answer' = solve' (simplify "input-19")
#time

