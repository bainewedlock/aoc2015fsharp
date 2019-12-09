open System
#load "input.fsx"

type Item = {
    SlotType : string
    Name : string
    Cost : int
    Damage : int
    Armor : int
  }

module Item = 
  let name item = item.Name
  let cost item = item.Cost
  let damage item = item.Damage
  let armor item = item.Armor

type Slot = {
    MinItems : int
    MaxItems : int
  }

let split (s: string) =
  s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
  |> Array.toList

let itemCatalog : Map<string, Item list> = 
  Input.asList "input-21-shop"
  |> List.filter ((<>) "")
  |> List.map (fun s -> s.Replace(" +", "+"))
  |> List.fold (fun (c, is) (line : string) ->
      match split line with
      | c'::_ when c'.EndsWith(":") ->
        (c'.TrimEnd(':'), is)
      | [na;co;da;ar] ->
        let item = {
          SlotType = c
          Name = na
          Cost = int co
          Damage = int da
          Armor = int ar }
        (c, item::is)
      | xs -> failwithf "unexpected line: %A" xs)
        ("?", [])
  |> snd
  |> List.groupBy (fun item -> item.SlotType)
  |> Map

type State =
  {
    Stock : string List
    FreeSlots : Map<string, Slot>
    GoldSpent : int
    Damage : int
    Armor : int
    Inventory : string List
  }
  with static member create =
        {
          Stock =
            itemCatalog
            |> Seq.collect (fun x -> x.Value |> List.map Item.name)
            |> Seq.toList
          FreeSlots = Map [
            ("Weapons", { MinItems = 1; MaxItems = 1 } )
            ("Armor"  , { MinItems = 0; MaxItems = 1 } )
            ("Rings"  , { MinItems = 0; MaxItems = 2 } ) ]
          GoldSpent = 0
          Damage = 0
          Armor = 0
          Inventory = []
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

let assignSlot name (items: Item list) state = 
  let itemSum x = List.sumBy x items
  let itemNames = items |> List.map Item.name
  { state with
      Stock = state.Stock |> List.except itemNames
      FreeSlots = state.FreeSlots.Remove name
      Armor = state.Armor + itemSum Item.armor
      Damage = state.Damage + itemSum Item.damage
      GoldSpent = state.GoldSpent + itemSum Item.cost
      Inventory = List.append state.Inventory itemNames
  }

let rec calcPermutations (s: State) = seq {
  if s.FreeSlots.IsEmpty then yield s
  else
    for kvp in s.FreeSlots do
      let slot = kvp.Value
      let items = itemCatalog.Item(kvp.Key)
      for xs in permuteSlot slot.MinItems slot.MaxItems items do
        let s' = assignSlot kvp.Key xs s
        yield! calcPermutations s'
}

type player = { Hitpoints : int; Damage : int; Armor : int }

let calcTicks att def : int option =
  match att.Damage - def.Armor with
  | 0 -> None
  | dps ->
    let t = def.Hitpoints / dps
    if t * dps < def.Hitpoints
    then Some (t + 1)
    else Some t

let fight human boss =
  match calcTicks human boss, calcTicks boss human with
  | _, None -> true
  | None, _ -> false
  | Some h, Some b -> h <= b

let boss = { Hitpoints = 104; Damage = 8; Armor = 1 }
calcPermutations State.create
|> Seq.filter (fun s ->
  let human = {
    Hitpoints = 100
    Damage = s.Damage
    Armor = s.Armor }
  fight human boss)
|> Seq.minBy (fun s -> s.GoldSpent)
