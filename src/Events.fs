module Events
open WebWorksheets.Types

type Event = 
    | StartEdit of Position
    | UpdateValue of Position * string
