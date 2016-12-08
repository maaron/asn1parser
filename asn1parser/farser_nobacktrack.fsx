
// Investigate breadth-first recursive descent parsers that require no backtracking
// The idea for this came from reading a tutorial paper about the uu-parsinglib, which can be 
// found here:
//
// http://www.cs.uu.nl/research/techreps/repo/CS-2008/2008-044.pdf
// https://github.com/sol/uu-parsinglib/blob/master/

// Details that won't translate to F# directly:
// - Use of GADTs in the Steps type
// - Dependence on Haskell's lazy evaluation, particularly in the 'best' function
// - Type classes used all over the place
//   - Perhaps either use interfaces or just not generalize as much
// - There was some issue with future parsers and monadic composition- the left side of the monad 
//   had to be a history parser, not a future one...  This I don't yet understand
// - There are uses of forall that I think don't translate, and wind up getting stuck in F#'s 
//   value restriction.  For instance, history parsers have the type 
//   "(a -> s -> Step r) -> s -> Step r", where the "r" type is forall, but must be an explicit 
//   parameter in F#.  Once the overall result is constrained, the same combinator instances 
//   can't be used in another context.
//   This is probably why FsAttoParsec uses a Parser interface instead of a function type (since 
//   the interface can define a generic method with an additional type parameter that isn't 
//   associated with the Parser type itself.)  Maybe another option is to drop the continuation 
//   style, since that is where the extra type parameter comes from?

// Main ideas
// - recognizers vs history parsers vs future parsers.  Every parser is a collection of all three 
//   types.
// - all parsers are "incremental" in a sense, not just "future" ones, which provide incremental 
//   results (or "witness" values).

(*
data T st a  = T  (forall r . (a  -> st -> Steps r)  -> st -> Steps       r  )  --   history parser
                  (forall r . (      st -> Steps r)  -> st -> Steps   (a, r) )  --   future parser
                  (forall r . (      st -> Steps r)  -> st -> Steps       r  )  --   recogniser 

uu-parsinglib Steps type applicable to history parsers only:

data Steps a where
    Step :: Steps a → Steps a
    Fail :: Steps a
    Done :: a → Steps a

T ph pf pr  <*> ~(T qh qf qr) = T ( \  k -> ph (\ pr -> qh (\ qr -> k (pr qr))))
*)

open System

module Step =
    type Step<'a> =
        | Done of 'a
        | Step of Lazy<Step<'a>>
        | Fail

module HistoryParser =
    open Step
    type HistoryParser<'s, 'a, 'r> = 
        HistoryParser of (('a -> 's -> Step<'r>) -> 's -> Step<'r>)

    let rec best s1 s2 =
        match s1, s2 with
        | Fail, r -> r
        | l, Fail -> l
        | Step l, Step r -> Step (lazy (best l.Value r.Value))
        | _ -> failwith "ambiguous grammar"

    (* Applicative interface *)
    let (<*>) (HistoryParser p) (HistoryParser q) = 
        HistoryParser (fun k -> p (fun f -> q (fun a -> k (f a))))

    let (<|>) (HistoryParser p) (HistoryParser q) = 
        HistoryParser (fun k i -> best (p k i) (q k i))

    let (<!>) f (HistoryParser p) =
        HistoryParser (fun k -> p (fun a -> k (f a)))

    let fail = 
        HistoryParser (fun k -> fun _ -> Fail)

    let retn a =
        HistoryParser (fun k -> k a)

open Step
open HistoryParser

let pfail = fail
let pretn1 = retn 1
let pretn2 = retn 2
let pretnf = retn ((+) 1)
let ptup = retn (fun a b -> (a,b))

let (HistoryParser p) = pretnf <*> pretn2

let parse s (HistoryParser p) =
    try
        Choice1Of2 (p (fun a b -> Done a) s)
    with
    | Failure e -> Choice2Of2 e

let r = p (fun a b -> Done a) ()

parse () (pretnf <*> pretn2)
parse () (pretnf <*> pfail)
parse () (pretn1 <|> pretn2)
parse () (pretn1 <|> pfail <|> pfail)
parse () (pretnf <*> pretn1)
parse () (id <!> pretn1)
parse () (ptup <*> (retn 1) <*> (retn 2))

let any = 
    let rec step k i =
        match i with
        | h :: t -> k h t
        | [] -> Fail

    HistoryParser step

let satisfy f =
    HistoryParser (fun k i ->
        match i with
        | h :: t when f h -> k h t
        | _ -> Fail)
        
let cnst c = satisfy ((=) c)

let ptup2 = retn (fun a b -> (a,b))

parse [1; 2] (ptup2 <*> any <*> any)
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