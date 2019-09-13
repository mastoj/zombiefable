module Types

type Face =
    | Punch
    | Shield
    | Shovel
    | Heal
    | Skull
    with member this.ClassName = (sprintf "%A" this).ToLower()

type ZombieId = string
type Zombie = { ZombieId: ZombieId; MaxHealth: int; LostHealth: int }

type DieId = string
type Die = { DieId: string; Faces: Face list; CurrentFace: Face}
type Rolls = { Max: int; Used: int }
type Game = {
    Zombies: Map<ZombieId, Zombie>
    Dice: Map<DieId, Die>
    Rolls: Rolls
}

module Game =
    let faces = [ Punch; Shield; Shovel; Punch; Heal; Skull ]
    let initialZombies =
        Map.empty |> Map.add "zombie-1" { ZombieId = "zombie-1"; MaxHealth = 5; LostHealth = 2 }

    let initialDice =
        [ 1 .. 5 ]
        |> List.map ((sprintf "die-%i") >> (fun id -> id, { DieId = id; Faces = faces; CurrentFace = Skull }))
        |> Map.ofList

    let intialGame =
        {
            Zombies = initialZombies
            Dice = initialDice
            Rolls = { Max = 3; Used = 1 }
        }
