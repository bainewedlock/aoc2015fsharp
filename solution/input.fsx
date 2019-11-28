open System.IO

let path n =
    let file = sprintf "input-%d.txt" n
    Path.Combine(__SOURCE_DIRECTORY__, "data", file)

let text n =
    File.ReadAllText(path n).Trim()

let lines n = 
    File.ReadAllLines(path n)
