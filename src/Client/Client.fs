module Client

open Elmish
open Elmish.React
open Feliz
open Types

type Model = {
    Game: Game
}

type Msg = unit

// module Server =

//     open Shared
//     open Fable.Remoting.Client

//     /// A proxy you can use to talk to server directly
//     let api : ICounterApi =
//       Remoting.createApi()
//       |> Remoting.withRouteBuilder Route.builder
//       |> Remoting.buildProxy<ICounterApi>
// let initialCounter = Server.api.initialCounter

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> = { Game = Game.intialGame }, Cmd.none

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> = currentModel, Cmd.none

module List =
    let interpose item list =
        let rec inner insert rem =
            seq {
                match insert, rem with
                | _, [] -> ()
                | true, _ -> yield item; yield! inner (not insert) rem
                | _, x::rem' -> yield x; yield! inner (not insert) rem'
            }
        inner false list |> Seq.toList

    let ofMap map = map |> Map.toList |> List.map snd

module ViewHelpers =
    let container' (classes: string list) (children: Fable.React.ReactElement list) =
        Html.div [
            prop.classes classes
            prop.children children
        ]
    let container (className: string) = container' [className]

module Components =
    open ViewHelpers

    let building x =
        container' ["building"; (sprintf "building-%i" x)] []

    let skyline = container "skyline" [for i in 0 .. 15 do yield building i]

    let dieWLock die =
        container "die-w-lock" [
            container "die" [
                container "cube"
                    (die.Faces |> List.mapi (fun i f -> container' ["face"; sprintf "face-%i" i; f.ClassName] []))
                container "clamp" [
                    container "lock" [
                        container "padlock" []
                    ]
                ]
            ]
        ]

    let rollsView {Max = maxRolls; Used = usedRolls} =
        let rollClasses i = if i < usedRolls then ["roll"; "used"] else ["roll"]
        container "rolls"
            [ for i in 0 .. (maxRolls-1) do yield container' (rollClasses i) []]

    let diceRow dice rolls =
        let dieList =
            dice
            |> List.map dieWLock
            |> List.interpose (container "dice-spacing" [])

        let children = dieList @ [
            rollsView rolls
        ]

        container "dice-row" children

    let zombieHealth maxHealth lostHealth =
        let hearts =
            [for i in 0 .. maxHealth-1 do
                let classes =
                    if i < lostHealth then ["heart"; "lost"] else ["heart"]
                yield container' classes []
            ]
        container "zombie-health" hearts

    let zombieView zombie =
        container "zombie-position" [
            container' ["zombie"; zombie.ZombieId] [zombieHealth zombie.MaxHealth zombie.LostHealth]
        ]

    let zombiesView zombies =
        container "zombies" (zombies |> List.ofMap |> List.map zombieView)

    let surfaceView zombies dice rolls = container "surface" [skyline; zombies; diceRow dice rolls]

    let gameView zombies dice rolls =
        surfaceView (zombiesView zombies) dice rolls

    let pageView model =
        let {Zombies = zombies; Dice = dice; Rolls = rolls} = model.Game
        container "page" [gameView zombies (dice |> List.ofMap) rolls]

open Components

let view (model : Model) (dispatch : Msg -> unit) = pageView model

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
