module day_4

open System.Security.Cryptography
open System.Text
open common

let md5 (data : byte array) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let hash key number =
    sprintf "%s%d" key number
    |> Encoding.ASCII.GetBytes
    |> md5

let test key min index =
    let h = hash key index
    let trimmed = h.TrimStart('0')
    match h.Length - trimmed.Length with
    | zero_count when zero_count >= min -> index
    | _ -> 0

let answer =
    let key = read_input 4
    Seq.initInfinite id
    |> Seq.map (test key 5)
    |> Seq.find (fun x -> x > 0)

let answer' =
    let key = read_input 4
    Seq.initInfinite id
    |> Seq.map (test key 6)
    |> Seq.find (fun x -> x > 0)



