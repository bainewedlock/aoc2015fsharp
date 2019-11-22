module day_2

open common
open System
open System.Text.RegularExpressions

let split_lines (s:String) =
    s.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)

let split (delim:Char) (s:String) =
    s.Split(delim)

let calc_paper = function
    | [l;w;h] -> 3 * l * w + 2 * w * h + 2 * l * h
    | _ -> failwith "cant calc paper"

let parse_line line =
    Regex.Match(line, @"(\d+)x(\d+)x(\d+)").Groups
    |> Seq.cast<Group>
    |> Seq.tail
    |> Seq.map (fun g -> g.Value |> Convert.ToInt32) 
    |> Seq.sort
    |> Seq.toList

let answer =
    read_input 2
    |> split_lines
    |> Array.map parse_line
    |> Array.map calc_paper
    |> Seq.sum

let calc_ribbon = function
    | [l;w;h] -> 2 * (l + w) + l*w*h
    | _ -> failwith "cant calc ribbon"

let answer' =
    read_input 2
    |> split_lines
    |> Array.map parse_line
    |> Array.map calc_ribbon
    |> Seq.sum


