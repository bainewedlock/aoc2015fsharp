open System.Text.RegularExpressions

#load "input.fsx"

let regexReplace
  (pattern : string)
  (replacement : string)
  (input : string) =
  Regex.Replace(input, pattern, replacement)

let reduce =
  regexReplace @"^""|""$" ""        >> // remove surrounding quotes
  regexReplace @"\\""" "?"          >> // replace \" with ?
  regexReplace @"\\\\" "?"          >> // replace \\ with ?
  regexReplace @"\\x[0-9a-f]{2,2}" "?" // replace \x?? with ?

let analyzeLine line =
  let reduced = reduce line
  let lineLength = String.length line
  let reducedLength = String.length reduced
  lineLength - reducedLength

let analyzeLines = Seq.map analyzeLine >> Seq.sum

let sample = Input.lines "8sample" |> analyzeLines

let answer = Input.lines "8" |> analyzeLines
