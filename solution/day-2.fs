module day_2

open common
open System
open System.Text.RegularExpressions

let split_lines (s:String) =
    s.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)

let split (delim:Char) (s:String) =
    s.Split(delim)

let calc_surface a b c =
    3 * a * b + 2 * b * c + 2 * a * c

let parse_line line =
    Regex.Match(line, @"(\d+)x(\d+)x(\d+)").Groups
    |> Seq.cast<Group>
    |> Seq.tail
    |> Seq.map (fun g -> g.Value |> Convert.ToInt32) 
    |> Seq.sort
    |> Seq.toList
    |> function
        | [a; b; c] -> calc_surface a b c

let answer =
    read_input 2
    |> split_lines
    |> Array.map parse_line
    |> Seq.sum
