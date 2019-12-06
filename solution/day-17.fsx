#load "input.fsx"

let rec permute n acc cs = [
  if n = 0 then yield acc
  else
    let cs' = cs |> List.filter ((>=) n)
    for (i, p) in cs' |> List.indexed do
      let rest = List.skip (i+1) cs'
      yield! permute (n-p) (p::acc) rest
]

let solve input x =
  Input.asList input
  |> List.map int
  |> permute x []
  |> List.length
solve "sample-17" 25
solve "input-17" 150

let solve' input x =
  Input.asList input
  |> List.map int
  |> permute x []
  |> List.countBy List.length
  |> List.minBy fst
  |> snd
solve' "sample-17" 25
solve' "input-17" 150

