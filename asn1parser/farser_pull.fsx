
// Try to make a better parser library (than fparsec) with following features:
// - Backtracking by default
// - Generalized char input type
// - Generalized fail values

module Input =
    type Position = int

    type Input<'e> = Position * InputNext<'e>
    
    and InputNext<'e> = 
        | End
        | Rest of 'e * (unit -> Input<'e>)

    let next = function
        | (pos, End) -> (pos, End)
        | (pos, Rest (_, getNext)) -> getNext ()

    let fromStr (s: string) =
        let rec next pos =
            if pos >= s.Length then (pos, End)
            else (pos, Rest (s.[pos], fun () -> next (pos + 1)))

        match s with
        | "" -> (0, End)
        | _ -> (0, Rest (s.[0], fun () -> next 1))

module List =
    open Input

    let toInput l =
        let rec toInputRec pos = function
            | [] -> pos, End
            | head :: tail -> pos, Rest (head, fun () -> toInputRec (pos + 1) tail)
        toInputRec 0 l

module Parser =
    open Input

    type Parser<'c, 'm, 'f> = Input<'c> -> Result<'c, 'm, 'f>

    and Result<'c, 'm, 'f> =
        | Match of 'm * Input<'c>
        | Fail of 'f

    let parse i p = p i

    let parseStr i p = parse (Input.fromStr i) p

    let any = function
        | (pos, Rest (c, next)) -> Match (c, next ())
        | _ -> Fail ()

    let cnst c = function
        | (pos, Rest (ci, next)) when ci = c -> Match (c, next ())
        | _ -> Fail c

    type WhereFailure<'m, 'f> =
        | ParserFail of 'f
        | ConditionFail of 'm
    
    let where predicate p i =
        match p i with
        | Match (m, rest) ->
            if predicate m then Match (m, rest)
            else Fail (ConditionFail m)
        | Fail f -> Fail (ParserFail f)

    let bind f p i =
        match p i with
        | Match (m, rest) -> f m i
        | _ as fail -> fail

    let retn m i = Match (m, i)

    let fail f i = Fail f

    let map f p i =
        match p i with
        | Match (m, rest) -> Match (f m, rest)
        | _ as fail -> fail

    let mapFail f p i =
        match p i with
        | Fail v -> Fail (f v)
        | _ as mat -> mat

    type Bookmark<'m, 'c> = Bookmark of 'm * Input<'c>
    
    let bookmark p i =
        match p i with
        | Match (m, next) -> Match (Bookmark (m, i), next)
        | Fail f -> Fail f

    let many (p: Parser<'c, 'm, 'f>) i: Result<'c, 'm list, 'f> =
        let (rest, results) = 
            (i, []) 
            |> Seq.unfold (fun (i, ms) -> 
                let pos, _ = i
                match p i with
                | Match (m, rest) -> 
                    let (posNext, _) = rest
                    if posNext > pos then 
                        let s = rest, m :: ms
                        Some (s, s)
                    else None
                | _ -> None)
            |> Seq.last

        Match (results |> Seq.rev |> Seq.toList, rest)

    let (<|>) p1 p2 i =
        match p1 i with
        | Match (m1, rest) -> Match (m1, rest)
        | Fail f1 ->
            match p2 i with
            | Fail f2 -> Fail (f1, f2)
            | Match (m2, rest) -> Match (m2, rest)

open Parser

cnst 'a' |> parse (List.toInput ['a'])
any |> where ((=) 'a') |> parseStr "b"
cnst 'a' |> where ((=) 'a') |> parseStr "a"
cnst 'a' |> where ((=) 'b') |> parseStr "a"
cnst 'a' |> where ((=) 'b') |> parseStr "b"

cnst 'a' |> where ((=) 'a') |> bookmark |> parseStr "a"

many (cnst 'a') |> parseStr "aaabbb"
many any |> parseStr "asdfawefawef"

(cnst 'a' <|> cnst 'b') |> parseStr "b"