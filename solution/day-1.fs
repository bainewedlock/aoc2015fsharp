module day_1

open common

let parse_char = function
    | ')' -> -1
    | '(' -> +1
    | c -> failwithf "unexpected char in input: %c" c

let steps =
    read_input 1
    |> Seq.map parse_char

let answer =
    steps
    |> Seq.sum

let answer' =
    steps
    |> Seq.scan (+) 0
    |> Seq.takeWhile (fun n -> n >= 0)
    |> Seq.length

