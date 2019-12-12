
type State =
  {
    PlayerHitpoints : int
    PlayerArmor : int
    PlayerMana : int
    BossHitpoints : int
    ManaSpent : int
    Effects : Map<string, int * (State -> State)>
    BossDamage : int
  }

let initialState =
  {
    BossHitpoints = 71
    BossDamage = 10
    PlayerHitpoints = 50
    PlayerMana = 500
    PlayerArmor = 0
    ManaSpent = 0
    Effects = Map.empty
  }

//let initialState =
//  {
//    BossHitpoints = 14
//    BossDamage = 8
//    PlayerHitpoints = 10
//    PlayerMana = 250
//    PlayerArmor = 0
//    ManaSpent = 0
//    Effects = Map.empty
//  }

let damageBoss damage (s: State) = {
  s with
    BossHitpoints =
      s.BossHitpoints - (max 1 damage) }

let healPlayer amount (s: State) = {
  s with
    PlayerHitpoints =
      s.PlayerHitpoints + amount
  }

let startEffect duration name f (s: State) = {
  s with
    Effects = s.Effects.Add(name, (duration-1, f))
  }

type Spell =
  {
    Cost : int
    Cast : State -> State
    Name : string
  }

module Spells =
  let private shieldEffect state = {
    state with PlayerArmor = 7
  }
  let private poisonEffect = damageBoss 3
  let private rechargeEffect (state: State) = {
    state with
      PlayerMana =
        state.PlayerMana + 101
  } 
  let catalog = [
    {
      Cost = 53
      Name = "Magic Missile"
      Cast = damageBoss 4
    }
    {
      Cost = 73
      Name = "Drain"
      Cast = (damageBoss 2 >> healPlayer 2)
    }
    {
      Cost = 113
      Name = "Shield"
      Cast = startEffect 6 "Shield" shieldEffect
    }
    {
      Cost = 173
      Name = "Poison"
      Cast = startEffect 6 "Poison" poisonEffect
    }
    {
      Cost = 229
      Name = "Recharge"
      Cast = startEffect 5 "Recharge" rechargeEffect
    }
  ]

let castable (state: State) (spell: Spell) =
  (state.PlayerMana >= spell.Cost) &&
  (not <| state.Effects.ContainsKey(spell.Name))

let castableSpells state = 
  Spells.catalog |> List.filter (castable state)

let damagePlayer (s: State) = {
  s with
    PlayerHitpoints =
      s.BossDamage - s.PlayerArmor
      |> max 1
      |> ((-) s.PlayerHitpoints)
  }

let applyEffects (s: State) =
  s.Effects
  |> Seq.map (fun kv -> snd kv.Value)
  |> Seq.fold (fun s' e -> e s') s

let reduceEffects (s: State) =
  let effects' =
    s.Effects
    |> Seq.choose (fun kv ->
      match kv.Value with
      | 1, _ ->
        None
      | duration, func ->
        Some (kv.Key, (duration-1, func)))
    |> Map
  { s with Effects = effects' }

let resetArmor (s: State) =
  { s with PlayerArmor = 0 }

let nextTurn (s: State) =
  s |> resetArmor |> applyEffects |> reduceEffects

let bossDead s = s.BossHitpoints <= 0
let playerDead s = s.PlayerHitpoints <= 0

type TurnResult =
  | BossDead
  | PlayerDead
  | Undecided

let spendMana m (s: State) = {
  s with
    PlayerMana = s.PlayerMana - m
    ManaSpent = s.ManaSpent + m}

let turn state spell =
  let s' =
    state
    |> nextTurn
    |> spell.Cast
    |> (spendMana spell.Cost)
    |> nextTurn
  if bossDead s' then (BossDead, s')
  else
    let s'' = s' |> damagePlayer
    if playerDead s'' 
    then (PlayerDead, s'')
    else (Undecided, s'')

let rec search round s = seq {
  for spell in castableSpells s do
    match turn s spell with
    | Undecided, s' -> yield! search (round+1) s'
    | BossDead, s' ->
      yield s'.ManaSpent
    | PlayerDead, s' ->
      ()
  }

let answer =
  search 0 initialState
  |> Seq.min

//SOLTION DOES NOT WORK  :/
