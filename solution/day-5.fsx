#load "input.fsx"

open System.Text.RegularExpressions

let rec containsVowels count (s: string) =
    if count = 0 then true
    else
    match s.IndexOfAny("aeiou".ToCharArray()) with
    | -1 -> false
    | index -> containsVowels (count-1) (s.Substring(index+1))

// It contains at least three vowels (aeiou only),
// like aei, xazegov, or aeiouaeiouaeiou
let condition1 (s: string) = containsVowels 3 s

// It contains at least one letter that appears
// twice in a row, like xx, abcdde (dd),
// or aabbccdd (aa, bb, cc, or dd).
let condition2 (s: string) = Regex.IsMatch(s, @"(.)\1")

// It does not contain the strings ab, cd, pq, or xy,
// even if they are part of one of the other requirements
let condition3 (s: string) =
    "ab cd pq xy".Split()
    |> Seq.exists(s.Contains)
    |> not

let isNice (s: string) =
    condition1 s && condition2 s && condition3 s

let answer =
    Input.lines "5"
    |> Seq.where isNice
    |> Seq.length

// It contains a pair of any two letters that appears
// at least twice in the string without overlapping,
// like xyxy (xy) or aabcdefgaa (aa),
// but not like aaa (aa, but it overlaps).
let condition1' (s: string) = Regex.IsMatch(s, @"(..).*\1")

// It contains at least one letter which repeats with
// exactly one letter between them, like xyx,
// abcdefeghi (efe), or even aaa.
let condition2' (s: string) = Regex.IsMatch(s, @"(.).\1")

let isNice' (s: string) =
    condition1' s && condition2' s

let answer' =
    Input.lines "5"
    |> Seq.where isNice'
    |> Seq.length


