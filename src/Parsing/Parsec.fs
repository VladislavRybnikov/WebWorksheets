namespace WebWorksheets.Parsing

[<Measure>]
type pos

type ParserResult<'a> = 'a option * int<pos>
type Parser<'a> = string * int<pos> -> ParserResult<'a>

[<AutoOpen>]
module Parsec = 
    let inline Success v i : ParserResult<'T> = Some v, i
    let inline Failure i   : ParserResult<'T> = None, i

    let inline (|MatchSuccess|MatchFailure|) (r : ParserResult<'T>) =
        match r with
        | None, i   -> MatchFailure i
        | Some v, i -> MatchSuccess (v, i)

    let bind (t : Parser<'T>) (uf : 'T -> Parser<'U>) : Parser<'U> = fun (s, i) ->
        match t (s, i) with
        | MatchFailure ii -> Failure ii
        | MatchSuccess (vv, ii) -> uf vv (s, ii)

    let inline (>>=) t uf = bind t uf

    let ret v : Parser<'T> = fun (s, i) -> Success v i
    let fail : Parser<'T> = fun (s, i) -> Failure i

    let map m t : Parser<'U> = t >>= fun v -> ret (m v)
    let inline (|>>) p m = map m p

    let opt (t : Parser<'T>) : Parser<'T option> = fun (s, i) ->
        match t (s, i) with
        | MatchFailure _ -> Success None i
        | MatchSuccess (vv, ii) -> Success (Some vv) ii

    let orElse t u : Parser<'T> =
        let ot = opt t
        ot >>= function Some tv -> ret tv | _ -> u

    let inline (<|>) t u = orElse t u

    let pair t u : Parser<'T*'U> = t >>= fun tv -> u >>= fun uv -> ret (tv, uv)
    let inline (.>>.) t u = pair t u

    let inline keepLeft t u : Parser<'T> = pair t u |>> fst
    let inline (.>>) t u = keepLeft t u

    let inline keepRight t u : Parser<'U> = pair t u |>> snd
    let inline (>>.) t u = keepRight t u

    let literal ch : Parser<unit> = fun (s, i) ->
        if int i < s.Length && ch = s.[int i] then Success () (i + 1<pos>)
        else Failure i

    let satisfy f : Parser<char> = fun (s, i) ->
        if int i < s.Length && f s.[int i] then Success s.[int i] (i + 1<pos>)
        else Failure i

    let many0 t : Parser<'T list> =
        let ot = opt t
        let rec loop pvs = ot >>= function Some pv -> loop (pv::pvs) | _ -> ret (pvs |> List.rev)
        loop []

    let many1 t : Parser<'T list> = many0 t >>= function [] -> fail | vs -> ret vs

    let chr c = literal c |>> fun () -> c
    let digit : Parser<char> = satisfy System.Char.IsDigit
    let letter : Parser<char> = satisfy System.Char.IsLetter
    let letterOrDigit : Parser<char> = satisfy System.Char.IsLetterOrDigit
    let whiteSpace : Parser<char> = satisfy System.Char.IsWhiteSpace

    let str p : Parser<string> = many0 p |>> (List.toArray >> System.String)
    let str2 f r : Parser<string> = f >>= fun ch -> many0 r |>> fun rest -> ch::rest |> List.toArray |> System.String
    let str1 f : Parser<string> = str2 f f

    let num : Parser<int> = str1 digit |>> System.Int32.Parse

    let parse (t : Parser<'T>) (s : string) : 'T option =
        match t (s, 0<pos>) with
        | MatchFailure _      -> None
        | MatchSuccess (v, _) -> Some v