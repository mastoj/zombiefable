module Client

open Elmish
open Elmish.React
open Feliz

type Model = unit

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
let init () : Model * Cmd<Msg> = (), Cmd.none

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

module Components =
    let container' (classes: string list) (children: Fable.React.ReactElement list) =
        Html.div [
            prop.classes classes
            prop.children children
        ]

    let container (className: string) =container' [className]

    let building x =
        let classes = ["building"; (sprintf "building-%i" x)]
        Html.div [
            prop.classes classes
        ]

    let skyline = container "skyline" [for i in 0 .. 15 do yield building i]

    let dieFaces = [ "punch"; "shield"; "shovel"; "punch"; "heal"; "skull"]
    let dieWLock die =
        container "die-w-lock" [
            container "die" [
                container "cube" [
                    for i in 0 .. 5 do yield container' ["face"; sprintf "face-%i" i; dieFaces.[i]] []
                ]
                container "clamp" [
                    container "lock" [
                        container "padlock" []
                    ]
                ]
            ]
        ]

    let diceRow dice =
        let dieList =
            dice
            |> List.map dieWLock
            |> List.interpose (container "dice-spacing" [])

        printfn "Die list: %A" dieList

        let rolls =
            container "rolls" [ for i in 0 .. 2 do yield container "roll" []]

        let children = dieList @ [
            rolls
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

    let zombie id maxHealth lostHealth =
        container "zombie-position" [
            container' ["zombie"; id] [zombieHealth maxHealth lostHealth]
        ]

    let zombies = container "zombies" [zombie "zombie-1" 5 3]

    let surface = container "surface" [skyline; zombies; diceRow [0 .. 4]]

    let page = container "page" [surface]

open Components

let view (model : Model) (dispatch : Msg -> unit) =

    Html.div [
        prop.className "page"
        prop.children [
            surface
        ]
    ]

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
