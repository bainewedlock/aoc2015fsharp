#load "input.fsx"

type State = 
  {
    programCounter : int
    registers : Map<string, int>
  }

let update (r: string) (f: int -> int) (s: State) =
  let v = s.registers.Item(r) |> f
  let p' = s.programCounter + 1
  let rs' = s.registers.Add(r, v)
  { s with programCounter = p'; registers = rs' }

let jump (r: string) (f: int->bool) (o: string) (s: State) =
  let v = s.registers.Item(r.TrimEnd(',')) |> f
  let p' = s.programCounter + (if v then int o else 1)
  { s with programCounter = p' }

let isEven x = x % 2 = 0

let always x = true

let parseInstruction (line: string) =
  match line.Split() |> Array.toList with
  | "hlf"::[r] -> update r (fun x -> x / 2)
  | "tpl"::[r] -> update r ((*) 3)
  | "inc"::[r] -> update r ((+) 1)
  | "jmp"::[offset] -> jump "a" always offset
  | "jie"::[r; offset] -> jump r isEven offset
  | "jio"::[r; offset] -> jump r ((=) 1) offset
  | _ -> failwithf "bad line: %s" line

let instructions =
  Input.asList "input-23"
  |> List.map parseInstruction
  |> List.indexed
  |> Map

let rec execute (state: State) =
  match instructions.TryFind(state.programCounter) with
  | None -> state
  | Some instruction -> instruction state |> execute

let answer = 
  {
    programCounter = 0
    registers = Map [("a", 0); ("b", 0)]
  }
  |> execute
  |> fun s -> s.registers.Item("b")

let answer' = 
  {
    programCounter = 0
    registers = Map [("a", 1); ("b", 0)]
  }
  |> execute
  |> fun s -> s.registers.Item("b")
