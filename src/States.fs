module States
open WebWorksheets.Types

type State = {
    Cols: char list
    Rows: int list
    Active: Position option
    Cells: Map<Position, string>
}
    with 
        static member initial = {
            Rows = [1 .. 15]
            Cols = ['A' .. 'K']
            Active = None
            Cells = Map.empty
        }

let init () = State.initial