open System.IO

let path suffix =
    let file = sprintf "input-%s.txt" suffix
    Path.Combine(__SOURCE_DIRECTORY__, "data", file)

let text n =
    File.ReadAllText(path n).Trim()

let lines n = 
    File.ReadAllLines(path n)
