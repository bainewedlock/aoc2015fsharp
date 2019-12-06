#load "input.fsx"

open System.Text.RegularExpressions

type Aunt = { nr : int; props : Map<string, int> }

let matchesRules (a : Aunt) =
  let shouldMatch expected key =
    match a.props.TryFind(key) with
    | Some x -> expected = x
    | None -> true
  "children" |> shouldMatch 3 &&
  "cats" |> shouldMatch 7 &&
  "samoyeds" |> shouldMatch 2 &&
  "pomeranians" |> shouldMatch 3 &&
  "akitas" |> shouldMatch 0 &&
  "vizslas" |> shouldMatch 0 &&
  "goldfish" |> shouldMatch 5 &&
  "trees" |> shouldMatch 3 &&
  "cars" |> shouldMatch 2 &&
  "perfumes" |> shouldMatch 1

let parse index line = {
    nr = index+1
    props = Map [ for m in
            Regex.Matches(line, @"(\w+): (\d+)")      
            do (m.Groups.[1].Value,
                int m.Groups.[2].Value) ]
  }

let solve fCriteria =
  Input.asList "input-16"
  |> List.mapi parse
  |> List.filter fCriteria

let answer = solve matchesRules

let matchesRules' (a : Aunt) =
  let shouldBe f expected key =
    match a.props.TryFind(key) with
    | Some x -> f x expected
    | None -> true
  "children" |> shouldBe (=) 3 &&
  "cats" |> shouldBe (>) 7 &&
  "samoyeds" |> shouldBe (=) 2 &&
  "pomeranians" |> shouldBe (<) 3 &&
  "akitas" |> shouldBe (=) 0 &&
  "vizslas" |> shouldBe (=) 0 &&
  "goldfish" |> shouldBe (<) 5 &&
  "trees" |> shouldBe (>) 3 &&
  "cars" |> shouldBe (=) 2 &&
  "perfumes" |> shouldBe (=) 1

let answer' = solve matchesRules'
