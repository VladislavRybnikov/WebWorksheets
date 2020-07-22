module Views
open Fable.React
open Fable.React.Props
open States
open Events
open WebWorksheets.Parsing.FormulaEvaluator
open WebWorksheets.Types
open Thoth.Json

let renderEditor dispatch pos value = 
    td [Style[Padding("0")]] [
        input [
            OnInput(fun s -> dispatch (UpdateValue(pos, s.Value)))
            Style[Width("90%"); Height("90%")]; 
            Id("celled"); Type("text"); DefaultValue(value)  
        ] 
    ]

let renderView dispatch (pos: Position) cells value = 
    let res = evaluate(value, cells)
     //if value <> "" then string (parse value) else value

    td [
        OnClick (fun _ -> dispatch(StartEdit pos))
    ] [str(res)]

let renderCell dispatch pos (state: State) = 
    state.Cells 
        |> Map.tryFind pos
        |> Option.defaultWith(fun () -> "")
        |> (
            match state.Active with
            | Some p when pos = p -> renderEditor dispatch p
            | _ -> renderView dispatch pos state.Cells
        )

let view state dispatch = 
    table [Style [Margin(20)]] [
        tbody[][
        yield tr [] [
            yield th [Style [Width(30)]] []
            for col in state.Cols -> th [] [ str (string col)]
        ]
        for row in state.Rows -> tr [] [
            yield th [Style [Width(30)]] [str (string(row))]
            for col in state.Cols -> renderCell dispatch (col, row) state
        ]
        ]
    ]

let update (event: Event) (state: State) = 
    match event with
    | StartEdit(pos) -> 
        { state with Active = Some pos}
    | UpdateValue(pos, value) -> 
        { state with Cells = state.Cells |> Map.add pos value }
