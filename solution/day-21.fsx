open System
#load "input.fsx"

type Item = {
    Name : string
    SlotName : string
    Cost : int
    Damage : int
    Armor : int
  }

module Item = 
  let cost item = item.Cost
  let damage item = item.Damage
  let armor item = item.Armor

type Slot = {
    Name : string
    MinItems : int
    MaxItems : int
  }

let itemCatalog : Map<string, Item list> = 
  let split (s: string) =
    s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
  Input.asList "input-21-shop"
  |> List.filter ((<>) "")
  |> List.map (fun s -> s.Replace(" +", "+"))
  |> List.fold (fun (c, is) (line : string) ->
      match split line with
      | c'::_ when c'.EndsWith(":") ->
        (c'.TrimEnd(':'), is)
      | [na;co;da;ar] ->
        let item = {
          SlotName = c
          Name = na
          Cost = int co
          Damage = int da
          Armor = int ar }
        (c, item::is)
      | xs -> failwithf "unexpected line: %A" xs)
        ("?", [])
  |> snd
  |> List.groupBy (fun item -> item.SlotName)
  |> Map

type State =
  {
    FreeSlots : Slot list
    GoldSpent : int
    Damage : int
    Armor : int
  }
  with static member create =
        {
          FreeSlots = [
            { Name = "Weapons"; MinItems = 1; MaxItems = 1 } 
            { Name = "Armor"  ; MinItems = 0; MaxItems = 1 }
            { Name = "Rings"  ; MinItems = 0; MaxItems = 2 } ]
          GoldSpent = 0
          Damage = 0
          Armor = 0
        }

let permuteSlot min max items = 
  let rec loop acc n items = [
    match n, items with
    | 0, _ -> ()
    | _, [] -> ()
    | _, _ ->
      if n = max && min = 0 then yield []
      for i in {0..items.Length-1} do
        let item = items.Item(i)
        if List.length acc + 1 >= min
          then yield item::acc
        let rest = List.skip (i+1) items
        yield! loop (item::acc) (n-1) rest
  ] 
  loop [] max items

let assignSlot name (items: Item list) state : State = 
  let itemSum x = List.sumBy x items
  { state with
      FreeSlots = List.filter (fun s -> s.Name <> name) state.FreeSlots
      Armor = state.Armor + itemSum Item.armor
      Damage = state.Damage + itemSum Item.damage
      GoldSpent = state.GoldSpent + itemSum Item.cost
  }

let rec calcPermutations (s: State) = seq {
  match s.FreeSlots with
  | [] -> yield s
  | x::_ ->
    let items = itemCatalog.Item(x.Name)
    for xs in permuteSlot x.MinItems x.MaxItems items do
      let s' = assignSlot x.Name xs s
      yield! calcPermutations s'
}

type player = { Hitpoints : int; Damage : int; Armor : int }

let calcTicks att def : int option =
  let dps = max 1 (att.Damage - def.Armor)
  let t = def.Hitpoints / dps
  if t * dps < def.Hitpoints
  then Some (t + 1)
  else Some t

let fight human boss =
  match calcTicks human boss, calcTicks boss human with
  | _, None -> true
  | None, _ -> false
  | Some h, Some b -> h <= b

type Outcome = { HumanWins : bool; GoldSpent : int }

let solve =
  let boss = { Hitpoints = 104; Damage = 8; Armor = 1 }
  calcPermutations State.create
  |> Seq.map (fun s ->
    let human = {
      Hitpoints = 100
      Damage = s.Damage
      Armor = s.Armor }
    { HumanWins = fight human boss; GoldSpent = s.GoldSpent})

let answer = 
  solve 
  |> Seq.filter (fun o -> o.HumanWins)
  |> Seq.minBy (fun o -> o.GoldSpent)

let answer' = 
  solve 
  |> Seq.filter (fun o -> o.HumanWins = false)
  |> Seq.maxBy (fun o -> o.GoldSpent)
