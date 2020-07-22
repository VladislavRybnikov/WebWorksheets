namespace WebWorksheets.Parsing
open WebWorksheets.Types

    module FormulaEvaluator = 
        let operator = chr '+' <|> chr '-' <|> chr '*' <|> chr '/'
        let number = num |>> Number 
        let ref = letter .>>. num |>> Reference
        let numOrRef = number <|> ref

        let binary = (numOrRef .>>. operator .>>. numOrRef) |>> fun ((l, op), r) -> Binary(l, op, r)
        let formula = chr '=' >>. binary
        let expr = number <|> formula <|> ref

        let tryParseWith (tryParseFunc: string -> bool * _) = tryParseFunc >> function
            | true, v    -> Some v
            | false, _   -> None

        let parseInt = tryParseWith System.Int32.TryParse

        let evalExpr ex (resolve: Position -> string option) = 
            match ex with
            | Number n -> n
            | Reference p -> p |> (resolve >> Option.bind(parseInt) >> Option.defaultWith(fun () -> 0))
            | _ -> -1

        let evalBinary l op r (resolve: Position -> string option) = 
            let f = 
                match op with
                | '+' -> fun (a: int, b: int) -> a + b
                | '-' -> fun (a: int, b: int) -> a - b
                | _ -> fun (_, _) -> -1

            let left = evalExpr l resolve
            let right = evalExpr r resolve
            f (left, right)
            
        let evaluate (str: string, cells: Map<Position, string>) = 
            let res = str |> parse expr
            let resolve = fun p -> cells |> Map.tryFind p
            match res with
            | Some e -> 
                match e with
                | Number n -> string n
                | Binary (l, op, r) -> string (evalBinary l op r resolve)
                | Reference p -> p |> (resolve >> Option.defaultWith(fun () -> "ERR"))
                | _ -> ""
            | None -> str