module App

open Views
open Elmish
open Elmish.React
open States

Program.mkSimple init update view
        |> Program.withReactBatched "elmish-app"
        |> Program.run