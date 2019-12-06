open System.IO

let path suffix =
    let file = sprintf "%s.txt" suffix
    Path.Combine(__SOURCE_DIRECTORY__, "data", file)

let asText n =
    File.ReadAllText(path n).Trim()

let asLines n = 
    File.ReadAllLines(path n)
