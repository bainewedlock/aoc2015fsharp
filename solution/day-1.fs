module day_1

open System.IO
open common


let x = "testiu 1 2 3"

let input = read_input 1

let parse_char c = match c with
    | ')' -> - 1
    | '(' -> + 1

let answer = read_input 1 |> Seq.map parse_char |> Seq.sum

