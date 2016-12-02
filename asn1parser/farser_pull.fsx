
// Try to make a better parser library (than fparsec) with following features:
// - Backtracking by default
// - Generalized char input type
// - Generalized fail values

// This style of parser combinator library also includes additional match information even in the 
// case of matching results.  For example, the many parser generates not only a list of matches, 
// but also the fail result of the final match attempt that failed.  This is important for failure 
// analysis.

module Input =
    type Position = int

    type Input<'e> = {
        pos: Position
        next: InputNext<'e>
        }
    
    and InputNext<'e> = 
        | End
        | Rest of 'e * (unit -> Input<'e>)

    let next i =
        match i.pos, i.next with
        | (pos, End) -> i
        | (pos, Rest (_, getNext)) -> getNext ()

    let fromStr (s: string) =
        let rec getNext pos =
            if pos >= s.Length then { pos = pos; next = End }
            else { pos = pos; next = Rest (s.[pos], fun () -> getNext (pos + 1)) }

        match s with
        | "" -> { pos = 0; next = End }
        | _ -> { pos = 0; next = Rest (s.[0], fun () -> getNext 1) }

    let getPos i = i.next

    let setPos pos i =
        let mutable im = i
        while pos > im.pos do im <- next im
        im

    let isEnd i = match i.next with | End -> true | _ -> false

    let rec filter predicate input =
        let skipItems input =
            let mutable i = input
            while (match i.next with | End -> false | Rest (c, _) -> not (predicate c)) do
                i <- next i
            i
        
        { input with 
            next = 
                match input.next with
                | End -> input.next
                | Rest (c, get) -> 
                    Rest (c, fun () -> 
                        skipItems (get ()) |> filter predicate) }
        |> skipItems

module List =
    open Input

    let toInput l =
        let rec toInputRec pos = function
            | [] -> { pos = pos; next = End }
            | head :: tail -> { pos = pos; next = Rest (head, fun () -> toInputRec (pos + 1) tail) }
        toInputRec 0 l

module Parser =
    open Input

    type Parser<'c, 'm, 'f> = Input<'c> -> Result<'c, 'm, 'f>

    and Result<'c, 'm, 'f> =
        | Match of 'm * Input<'c>
        | Fail of 'f

    let parse i p = p i

    let parseStr i p = parse (Input.fromStr i) p

    let any i =
        match i.next with
        | Rest (c, getNext) -> Match (c, getNext ())
        | _ -> Fail ()

    let cnst c i =
        match i.next with
        | Rest (ci, getNext) when ci = c -> Match (c, getNext ())
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
        | Match (m, rest) -> Match (m, rest)

    type Bookmark<'m, 'c> = Bookmark of 'm * Input<'c>
    
    let bookmark p i =
        match p i with
        | Match (m, next) -> Match (Bookmark (m, i), next)
        | Fail f -> Fail f

    type FoldLastAttempt<'f> =
        | ZeroLength
        | ParserFail of 'f
    
    type FoldResult<'m, 'f> = FoldResult of 'm * FoldLastAttempt<'f>

    module FoldResult =
        let map f fr = 
            let (FoldResult (r, fail)) = fr
            FoldResult (f r, fail)

    let fold folder init p i: Result<'c, FoldResult<'m, 'f>, unit> =
        let (rest, results, lastAttempt) = 
            let gen state =
                let (i, results, lastAttempt) = state
                match lastAttempt with
                | Some f -> None
                | None ->
                    match p i with
                    | Match (m, rest) -> 
                        let s = 
                            rest, folder results m, if rest.pos > i.pos then None else Some (ZeroLength)
                        Some (s, s)
                    | Fail f -> 
                        let s = i, results, Some (ParserFail f)
                        Some (s, s)

            (i, init, None) |> Seq.unfold gen |> Seq.last

        Match (FoldResult (results, lastAttempt.Value), rest)

    let many p = 
        fold (fun r m -> m :: r) [] p 
        |> map (FoldResult.map List.rev)
    
    let (<|>) p1 p2 i =
        match p1 i with
        | Match (m1, rest) -> Match (m1, rest)
        | Fail f1 ->
            match p2 i with
            | Fail f2 -> Fail (f1, f2)
            | Match (m2, rest) -> Match (m2, rest)

    type SequenceFailure2<'f1, 'f2, 'm1> = Choice<'f1, 'm1 * 'f2>

    type SequenceFailure3<'f1, 'f2, 'f3, 'm1, 'm2> = Choice<'f1, 'm1 * 'f2, 'm1, 'm2, 'f3>

    let pipe2 p1 p2 f i =
        match p1 i with
        | Fail f1 -> Fail (Choice1Of2 f1)
        | Match (m1, rest) ->
            match p2 rest with
            | Fail f2 -> Fail (Choice2Of2 (m1, f2))
            | Match (m2, rest2) -> Match (f m1 m2, rest2)

    let pipe3 p1 p2 p3 f i =
        match p1 i with
        | Fail f1 -> Fail (Choice1Of3 f1)
        | Match (m1, rest) ->
            match p2 rest with
            | Fail f2 -> Fail (Choice2Of3 (m1, f2))
            | Match (m2, rest2) -> 
                match p3 rest2 with
                | Fail f3 -> Fail (Choice3Of3 (m1, m2, f3))
                | Match (m3, rest3) -> Match (f m1 m2 m3, rest3)

    let tuple2 p1 p2 = pipe2 p1 p2 (fun a b -> a, b)
    let tuple3 p1 p2 p3 = pipe3 p1 p2 p3 (fun a b c -> a, b, c)

    let tmap2 f (p1, p2) = pipe2 p1 p2 f
    let tmap3 f (p1, p2, p3) = pipe3 p1 p2 p3 f

    let (>>.) p1 p2 = pipe2 p1 p2 (fun _ b -> b)

open Parser

cnst 'a' |> parse (List.toInput ['a'])
any |> where ((=) 'a') |> parseStr "b"
cnst 'a' |> where ((=) 'a') |> parseStr "a"
cnst 'a' |> where ((=) 'b') |> parseStr "a"
cnst 'a' |> where ((=) 'b') |> parseStr "b"

cnst 'a' |> where ((=) 'a') |> bookmark |> parseStr "a"

many (cnst 'a') |> parseStr "aaabbb"
many any |> parseStr "asdfawefawef"
many (cnst 'a' <|> cnst 'b') |> parseStr "aaabbbccc"

(cnst 'a', cnst 'b') |> tmap2 (fun a b -> sprintf "%A then %A" a b) |> parseStr "ab"
(cnst 'a', cnst 'b') |> tmap2 (fun a b -> sprintf "%A then %A" a b) |> parseStr "ac"

cnst 'a' >>. cnst 'b' |> parseStr "ab"
cnst 'a' >>. cnst 'b' |> parseStr "b"
cnst 'a' >>. cnst 'b' |> parseStr "aa"

cnst 'a' >>. cnst 'b' 
|> mapFail (function
    | Choice1Of2 f -> "first failed" 
    | Choice2Of2 _ -> "second failed")
|> parseStr "aa"

let f1 a = a
let f2 a = a, a
let f3 f1 f2 a = f2 (f1 a)
let f3' f1 f2 = f1 >> f2
f3' f1 f2 1
(f1, f2, 3) |||> f3'

List.toInput [0; 1; 0; 2; 0; 3; 0] |> Input.filter ((<>) 0)
|> Input.next
|> Input.next
|> Input.next
