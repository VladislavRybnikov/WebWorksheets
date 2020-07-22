namespace WebWorksheets.Types

type Expr = 
    | Number of int
    | Binary of Expr * char * Expr
    | Reference of (char * int)
    | Err of string