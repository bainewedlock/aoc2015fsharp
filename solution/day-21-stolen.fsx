

type Character =
    {
        Name      : string
        HitPoints : int
        Damage    : int
        Armor     : int
    }

type Equipment =
    {
        Cost   : int
        Damage : int
        Armor  : int
    }

type Shop =
    {
        Weapons : Equipment[]
        Armors  : Equipment[]
        Rings   : Equipment[]
    }

let you = 
    {
        Name      = "you"
        HitPoints = 100
        Damage    = 0
        Armor     = 0
    }

// set your input here
let boss =
    {
        Name      = "boss"
        HitPoints = 104
        Damage    = 8
        Armor     = 1
    }

let equipment cost damage armor =
    { Cost = cost; Damage = damage; Armor = armor }

let shop =
    {
        Weapons = 
            [|
                equipment 8  4 0
                equipment 10 5 0
                equipment 25 6 0
                equipment 40 7 0
                equipment 74 8 0
            |]
        Armors =
            [|
                equipment 13  0 1
                equipment 31  0 2
                equipment 53  0 3
                equipment 75  0 4
                equipment 102 0 5
            |]
        Rings =
            [|
                equipment 25  1 0
                equipment 50  2 0
                equipment 100 3 0
                equipment 20  0 1
                equipment 40  0 2
                equipment 80  0 3
            |]
    }

let weaponChoices = 
    shop.Weapons |> Seq.map (fun w -> [| w |])

let armorChoices = 
    seq {
        yield [||] // no armor
        for a in shop.Armors -> [| a |]
    }

let ringChoices =
    seq {
        yield [||] // no ring
        for r in shop.Rings -> [| r |] // 1 ring
        for i in 0..shop.Rings.Length-1 do
            for j in i+1..shop.Rings.Length-1 do
                // 2 rings
                yield [| shop.Rings.[i]; shop.Rings.[j] |]
    }

let equipmentCombos =
    seq {
        for w in weaponChoices do
            for a in armorChoices do
                for r in ringChoices do
                    yield Array.concat [| w; a; r |]
    }

let runSim boss you =
    let rec loop (attacker : Character) (defender : Character) =
        let damage = max 1 (attacker.Damage - defender.Armor)
        let newHP  = defender.HitPoints - damage
        if newHP <= 0
        then attacker.Name
        else loop { defender with HitPoints = newHP } attacker

    loop you boss

let answer =
    equipmentCombos
    |> Seq.filter (fun equipments -> 
        { you with 
            Damage = equipments |> Array.sumBy (fun x -> x.Damage)
            Armor  = equipments |> Array.sumBy (fun x -> x.Armor) }
        |> runSim boss
        |> (=) "boss")
    |> Seq.maxBy (Array.sumBy (fun x -> x.Cost))
    //|> Seq.max


//let csv =
//    equipmentCombos
//    |> Seq.map (fun equipments -> 
//        [ equipments |> Array.sumBy (fun x -> x.Damage);
//          equipments |> Array.sumBy (fun x -> x.Armor);
//          equipments |> Array.sumBy (fun x -> x.Cost) ]
//        |> List.map string
//        |> String.concat ";")
//    |> Seq.sortBy id
//    |> fun t -> System.IO.File.WriteAllLines(@"c:\users\wboec\Desktop\monk.csv", t)
