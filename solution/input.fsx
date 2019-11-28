open System.IO

let text n =
    let solution_directory = Directory.GetParent(__SOURCE_DIRECTORY__).FullName
    let file = sprintf "input-%d.txt" n
    let input_path = Path.Combine(solution_directory, "data", file)
    File.ReadAllText(input_path).Trim()

let lines n = 
    (text n).Split('\n')
    |> Array.map (fun s -> s.Trim())
