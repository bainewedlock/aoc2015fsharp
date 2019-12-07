#load "input.fsx"

open System.Text.RegularExpressions

let rect left top right bottom =
    set [
    for x in { left..right } do
    for y in { top..bottom } do
        yield (x, y)
    ]

let parseRect (s: string) =
    let groups = Regex.Match(s, @"(\d+),(\d+) through (\d+),(\d+)").Groups
    [ for g in groups -> g.Value ]
    |> List.tail
    |> List.map int
    |> fun [a;b;c;d] -> rect a b c d

let (|StartsWith|_|) (substring: string) (text: string) = 
    if text.StartsWith(substring) then Some (parseRect text)
    else None

#time
let answer =
    let state = Array2D.init 1000 1000 (fun x y -> 0)
    let turnOn rect = for (x, y) in rect do state.[x, y] <- 1
    let turnOff rect = for (x, y) in rect do state.[x, y] <- 0
    let toggle rect = for (x, y) in rect do state.[x, y] <- 1 - state.[x, y]
    let handleLine = function
        | StartsWith "turn on" rect -> turnOn rect
        | StartsWith "turn off" rect -> turnOff rect
        | StartsWith "toggle" rect -> toggle rect
    Input.asArray "input-6"
    |> Seq.iter handleLine
    state
    |> Seq.cast<int>
    |> Seq.sum
#time


#time
let answer' =
    let state = Array2D.init 1000 1000 (fun x y -> 0)
    let turnOn rect = for (x, y) in rect do state.[x, y] <- state.[x, y] + 1
    let turnOff rect = for (x, y) in rect do state.[x, y] <- max 0 (state.[x, y] - 1)
    let toggle rect = for (x, y) in rect do state.[x, y] <- state.[x, y] + 2
    let handleLine = function
        | StartsWith "turn on" rect -> turnOn rect
        | StartsWith "turn off" rect -> turnOff rect
        | StartsWith "toggle" rect -> toggle rect
    Input.asArray "input-6"
    |> Seq.iter handleLine
    state
    |> Seq.cast<int>
    |> Seq.sum
#time
