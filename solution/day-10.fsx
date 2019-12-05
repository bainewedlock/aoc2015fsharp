open System

#load "input.fsx"

let read (input : string) =
  input
  |> Seq.fold (fun acc x ->
    match acc with
    | (n, x')::history when x = x' ->
      (n+1, x')::history
    | _ -> (1, x)::acc) []
  |> List.rev
  |> Seq.collect (fun (n, x) -> sprintf "%d%c" n x)
  |> fun items -> String.Concat(items)

{1..50}
|> Seq.fold (fun last _ -> read last) (Input.text "10")
|> Seq.length
