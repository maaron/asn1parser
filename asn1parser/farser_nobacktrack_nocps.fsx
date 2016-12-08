

// Try to modify farser_nobacktrack so that continuation passing style is not used, which 
// hopefully avoids the forall requirement in the parser's return type.

// Interesting: As currently written, the retn (Applicative pure/return) parser doesn't work as 
// expected, because all parsers require input before they can be run, whereas retn doesn't 
// require or consume anything.  Is this bad?  Maybe not...

open System

module Parser =
    type Step<'s, 'a> =
        | Done of 'a
        | Step of Parser<'s, 'a>
        | Fail

    and Parser<'s, 'a> = 
        Parser of ('s -> Step<'s, 'a>)

    (* Applicative interface *)
    let (<*>) (Parser p) (Parser q) = 
        let rec parseSecond a p s =
            match p s with
            | Done b -> Done (a b)
            | Fail -> Fail
            | Step (Parser pnext) -> Step (Parser (parseSecond a pnext))

        let rec parse p s =
            match p s with
            | Done a -> Step (Parser (parseSecond a q))
            | Fail -> Fail
            | Step (Parser pnext) -> Step (Parser (parse pnext))
        Parser (parse p)

    let (<|>) (Parser p) (Parser q) = 
        let rec parse p q s =
            match p s, q s with
            | Fail, r -> r
            | l, Fail -> l
            | Step (Parser l), Step (Parser r) -> 
                Step (Parser (parse l r))
            | _ -> Fail
        Parser (parse p q)

    let (<!>) f (Parser p) =
        let rec parse p s =
            match p s with
            | Done a -> Done (f a)
            | Fail -> Fail
            | Step (Parser pnext) -> Step (Parser (parse pnext))
        Parser (parse p)

    let fail = 
        Parser (fun _ -> Fail)

    let retn a =
        Parser (fun _ -> Done a)

open Parser

let pfail = fail
let pretn1 = retn 1
let pretn2 = retn 2
let pretnf = retn ((+) 1)
//let ptup = retn (fun a b -> (a,b))

let (Parser p) = pretnf <*> pretn2

let parse s (Parser p) = p s

let thenParse s step =
    match step with
    | Done a -> Done a
    | Fail -> Fail
    | Step p -> parse s p

parse 1 (pretnf <*> pretn2)
parse 1 (pretnf <*> pfail)
parse 1 (pretn1 <|> pretn2)
parse 1 (pretn1 <|> pfail <|> pfail)
parse 1 (pretnf <*> pretn1)
parse 1 (id <!> pretn1)
//parse () (ptup <*> (retn 1) <*> (retn 2))

let any = 
    Parser (fun s -> Done s)

let satisfy f =
    Parser (fun s ->
        match f s with
        | true -> Done s
        | _ -> Fail)
        
let cnst c = satisfy ((=) c)

let tup a b = (a, b)

(tup <!> any <*> any)
|> parse 1
|> thenParse 2
(*
parse [1; 2] (ptup2 <*> any <*> (cnst 2))
parse [1; 2] (ptup2 <*> any <*> (cnst 1))
parse [1; 2; 3] (ptup2 <*> any <*> any)
parse [1; 2] (ptup2 <*> (cnst 1) <*> (cnst 2))

parse [1; 2; 3] (ptup2 <*> (satisfy ((>) 2)) <*> (satisfy ((>) 3)))

let p11 = (fun a b -> (a,b)) <!> cnst 1 <*> cnst 1
let p12 = (fun a b -> (a,b)) <!> cnst 1 <*> cnst 2
let p11or12 = p11 <|> p12

let asdf = (fun a b -> (a,b)) <!> p11or12 <*> p11or12
parse [1; 2; 1; 2] asdf
*)

let parseList p l =
    let gen (s, l) =
        match s with
        | Done a -> None
        | Fail -> None
        | Step p -> 
            match l with
            | [] -> Some (Fail, (Fail, []))
            | h :: t -> 
                let step = parse h p
                Some (step, (step, t))
    List.unfold gen (Step p, l)

let asdf p = List.fold (fun s c -> thenParse c s) (Step p)

[1; 2]
|> parseList (tup <!> any <*> any)

[1; 3]
|> asdf (tup <!> (cnst 1 <|> cnst 2) <*> (cnst 1 <|> cnst 2))

['a'; 'b']
|> asdf (tup <!> (cnst 'a' <|> cnst 'b') <*> (cnst 'a' <|> cnst 'b'))