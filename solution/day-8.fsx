open System.Text.RegularExpressions

#load "input.fsx"

let regexReplace
  (pattern : string)
  (replacement : string)
  (input : string) =
  Regex.Replace(input, pattern, replacement)

let decode =
  regexReplace @"^""|""$" ""        >> // remove surrounding quotes
  regexReplace @"\\""" "?"          >> // replace \" with ?
  regexReplace @"\\\\" "?"          >> // replace \\ with ?
  regexReplace @"\\x[0-9a-f]{2,2}" "?" // replace \x?? with ?

let analyzeLine change line =
  let changed = change line
  let lineLength = String.length line
  let reducedLength = String.length changed
  lineLength - reducedLength

let analyzeFile change file =
  Input.asArray file
  |> Seq.map (analyzeLine change)
  |> Seq.sum

let sample = analyzeFile decode "sample-8"
let answer = analyzeFile decode "input-8"

let encode =
  regexReplace @"""" "??"           >> // escape "
  regexReplace @"\\" "??"           >> // replace \
  regexReplace @"^.*$" "?$0?"          // surround

let sample' = - analyzeFile encode "sample-8"
let answer' = - analyzeFile encode "input-8"
