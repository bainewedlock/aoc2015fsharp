#load "input.fsx"

let parse_char = function
    | '^' -> (0, 1)
    | '>' -> (1, 0)
    | 'v' -> (0, -1)
    | '<' -> (-1, 0)
    | c -> failwithf "bad char: '%c'" c

let walk (x, y) (x', y') = (x+x', y+y')

let answer =
    Input.asText "3"
    |> Seq.map parse_char
    |> Seq.scan walk (0, 0)
    |> Seq.distinct
    |> Seq.length

let multi_walk states steps =
    List.zip states steps
    |> List.map (fun (state, step) -> walk state step)

let answer' =
    Input.asText "3"
    |> Seq.map parse_char
    |> Seq.chunkBySize 2
    |> Seq.map Array.toList
    |> Seq.scan multi_walk [(0, 0); (0, 0)]
    |> Seq.concat
    |> Seq.distinct
    |> Seq.length

