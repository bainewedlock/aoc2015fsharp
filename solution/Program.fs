open System
open System.IO

let read_input n =
    let solution_directory = Directory.GetParent(__SOURCE_DIRECTORY__).FullName
    let file = sprintf "input-%d.txt" n
    let input_path = Path.Combine(solution_directory, "data", file)
    File.ReadAllText(input_path)

let input = read_input 1

let foo x = sprintf "-%c-" x

let answer = read_input 1 |> Seq.fold (fun result c ->
    match c with
    | ')' -> result - 1
    | '(' -> result + 1
    | _ ->
        printfn "test"
        result ) 0




[<EntryPoint>]
let main argv =
    0 // return an integer exit code



