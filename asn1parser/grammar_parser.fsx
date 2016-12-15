
// Parse a grammar (bnf, abnf, etc) and try to write functions to process it into a parser

// Can we identify syntactical tokens and make sure they aren't present in the AST?
// For example, a rule for comma-separated lists enclosed in braces:
//
// rule ::= '{' subrule (',' subrule)* '}'
//
// The comma and braces aren't part of the semantics- they just serve to provide structure so 
// that the construct can be recognized.  A natural choice for the resulting AST is:
//
// type ruleAst = subruleAst list
//
// Or possibly:
//
// type ruleAst = subruleAst * subruleAst list
//
// The latter also captures the fact that there must be at least one subrule.
//
// The other thing that is happening is that the token constants '{', '}', and ',' are being 
// dropped.  So maybe this is a start to a transform:
//
// let rec makeAst rule =
//     match rule with 
//     | Sequence (Const a) (Const b) -> None
//     | Sequence (Const a) b -> makeAst b
//     | Sequence a (Const b) -> makeAst a
//

open System.Globalization

module Tuple =
    let map f (a, b) = (f a, f b)
    let mapSnd f (a, b) = (a, f b)
    let mapFst f (a, b) = (f a, b)

// AST for grammar rules:
type AlternateExpression = Alternate of SequenceExpression list

and SequenceExpression = Sequence of ExpressionTerm list

and ExpressionTerm =
    | Reference of string
    | Constant of string
    | Expression of AlternateExpression
    | ZeroOrMore of AlternateExpression
    | OneOrMore of AlternateExpression
    | Optional of AlternateExpression
    | Empty

type Rule = string * AlternateExpression

type Grammar = Rule list

let escapeString s =
    s |> String.collect (fun c -> 
        match c with
        | '\r' -> "\\r"
        | '\n' -> "\\n"
        | '\t' -> "\\t"
        | '"' -> "\\\""
        | _ -> sprintf "%c" c)

let rec prettyProduction (Alternate alts) =
    System.String.Join (" | ", alts |> List.map prettySequence)

and prettyTerm = function
    | Reference r -> r
    | Constant c -> "\"" + escapeString c + "\""
    | Empty -> "empty"
    | Expression e -> "(" + prettyProduction e + ")"
    | ZeroOrMore (Alternate [Sequence [term]]) -> prettyTerm term + "+"
    | ZeroOrMore e -> "(" + prettyProduction e + ")*"
    | OneOrMore (Alternate [Sequence [term]]) -> prettyTerm term + "*"
    | OneOrMore e -> "(" + prettyProduction e + ")+"
    | Optional (Alternate [Sequence [term]]) -> prettyTerm term + "?"
    | Optional e -> "(" + prettyProduction e + ")?"

and prettySequence (Sequence s) =
    System.String.Join(" ", s |> List.map prettyTerm)

and prettyRule (name, production) =
    name + " ::= " + prettyProduction production

and prettyGrammar g =
    g
    |> Seq.map prettyRule 
    |> String.concat "\n"

// Bnf grammar
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open FParsec

let idNonStartChar c =
    isAsciiLetter c
    || isDigit c
    || c = '_'
    || c = '-'

let ident = identifier (IdentifierOptions (isAsciiIdContinue = idNonStartChar))

let escapedChar = 
    pchar '\\' >>. anyChar |>> function
        | 'n' -> '\n'
        | 't' -> '\t'
        | 'r' -> '\r'
        | other -> other

let quotedChar = escapedChar <|> (satisfy ((<>) '"'))

let quotedString = between (pchar '"') (pchar '"') (many1Chars quotedChar)

let linearSpaceChar = satisfy (fun c -> c = ' ' || c = '\t')

let linearSpace = many (attempt (newline .>> followedBy spaces1) <|> linearSpaceChar)

runParserOnString (linearSpace .>>. many anyChar) () "test" "   \nb \na"

let production, productionRef = createParserForwardedToRef()

let term =
    (between (pchar '(' .>> linearSpace) (pchar ')') production |>> Expression)
    <|> (ident |>> (fun s ->  if s = "empty" then Empty else Reference s))
    <|> (quotedString |>> Constant)

let postfixedTerm =
    term .>>. many (anyOf "*+?")
    |>> (fun (t, ops) -> 
        ops |> List.fold (fun t op ->
            match op with
            | '?' -> Optional (Alternate [Sequence [t]])
            | '*' -> ZeroOrMore (Alternate [Sequence [t]])
            | '+' -> OneOrMore(Alternate [Sequence [t]])
            | _ -> failwith <| sprintf "unexpected token '%A'" op)
            t)

let sequence =
    (many1 (postfixedTerm .>> linearSpace) |>> Sequence)

productionRef.Value <- 
    ((sepBy1 (sequence .>> linearSpace) (pchar '|' .>> linearSpace)) |>> Alternate)

let rule = 
    ident .>> linearSpace .>> (pstring "::=") .>> linearSpace .>>. production .>> (opt skipNewline)

let alternates (Alternate alts) = alts

let terms (Sequence terms) = terms

let grammar = spaces >>. many1 rule

let parse s =
    runParserOnString grammar () "test" s

let check s expectedAst =
    match parse s with
    | Success (ast, _, _) ->
        if ast <> expectedAst then 
            printf "Mismatch: %A\nExpected: %A\nActual:   %A\n" s expectedAst ast
        else
            printf "good\n"
    | Failure (e, a, b) ->
        printf "Parse failed: %s\n" e

parse "rule ::= anotherrule ruleasdf | rule2 rule3"
parse "rule ::= anotherrule ( ruleasdf | rule2 ) rule3"
parse "rule ::= anotherrule(ruleasdf|rule2)rule3"
parse "rule ::= \"token\""
parse "rule ::= \"tok\\\"en\""
parse "rule ::= \"token\"\nrule2 ::= foo"
parse "A ::= (B | C) D"

// Test quoted strings
check "rule ::= \"tok\\\"en\"" <| ["rule", Alternate [Sequence [Constant "tok\"en"]]]
check "rule ::= \"token\"" <| ["rule", Alternate [Sequence [Constant "token"]]]

check "rule ::= anotherrule" <| 
    ["rule", Alternate [Sequence [Reference "anotherrule"]]]

check "rule ::= sub1 | sub2" <| 
    ["rule", Alternate [
                Sequence [Reference "sub1"]
                Sequence [Reference "sub2"]]
    ]

// Combine separate rules with equal names into a single rule with multiple alternates
let groupAlternates =
    List.groupBy fst
    >> List.map (fun (name, rules) ->
        let production =
            rules
            |> List.collect (
                snd
                >> alternates 
                >> List.distinct) 
            |> Alternate
        name, production)

let startsWithReference rule def =
    match def with
    | Sequence (Reference r :: _) when r = rule -> true
    | _ -> false

let (|MatchDirectLeftRecursive|_|) (name, production) =
    let (recAlts, nonRecAlts) = alternates production |> List.partition (startsWithReference name)
    match recAlts with
    | [] -> None
    | _ -> Some ((name, production), recAlts, nonRecAlts)

let isIdentityRule name = function
    | Sequence [Reference r] when r = name -> true
    | _ -> false

let discardEqualityRules (name, production) = 
    let production = 
        production 
        |> alternates
        |> List.filter (isIdentityRule name >> not)
        |> Alternate
    name, production

let addReference reference (Sequence sequence) = Sequence (sequence @ [Reference reference])

let removeFirstReference (Sequence s) = Sequence (List.tail s)

let replaceDirectLeftRecursiveRule = 
    discardEqualityRules >> function
    | MatchDirectLeftRecursive ((name, production), recAlts, nonRecAlts) ->
        let newRule = name + "'"
        let oldRuleAlts = nonRecAlts |> List.map (addReference newRule)
        let newRuleAlts = 
            (recAlts 
                |> List.map removeFirstReference 
                |> List.map (addReference newRule))
             @ [Sequence [Empty]]

        [ name, Alternate oldRuleAlts
          newRule, Alternate newRuleAlts ]

    | _ as skip -> [skip]

// Returns modified and new rule separately
let replaceDirectLeftRecursiveRule2 = 
    discardEqualityRules >> function
    | MatchDirectLeftRecursive ((name, production), recAlts, nonRecAlts) ->
        let newRule = name + "'"
        let oldRuleAlts = nonRecAlts |> List.map (addReference newRule)
        let newRuleAlts = 
            (recAlts 
                |> List.map removeFirstReference 
                |> List.map (addReference newRule))
             @ [Sequence [Empty]]

        (name, Alternate oldRuleAlts), Some (newRule, Alternate newRuleAlts)

    | _ as skip -> skip, None

let tryFindRule name =
    List.tryPick (fun (n, p) -> 
        if name = n then Some p else None)

let addTerms terms (Sequence s) = Sequence (List.append s terms)

let substituteSequence rules s =
    match s with
    | Sequence (Reference r :: t) -> 
        match tryFindRule r rules with
        | Some prod -> alternates prod |> List.map (addTerms t)
        | None -> [s]
    | _ -> [s]

let substituteRules rules (name, production) =
    let production = 
        alternates production
        |> List.collect (substituteSequence rules)
        |> Alternate
    name, production

let substituteAllRules rules rule =
    rule 
    |> Seq.unfold (fun r ->
        let newRule = substituteRules rules r
        if newRule = r then None else Some (newRule, newRule))
    |> Seq.tryLast
    |> function Some r -> r | None -> rule

let removeLeftRecursion =
    List.fold (fun prevRules rule -> 
        substituteAllRules prevRules rule
        |> replaceDirectLeftRecursiveRule
        |> List.append prevRules) []

// Returns new rules separately from the modified ones
let removeLeftRecursionSplit =
    ([], []) 

    // Build a list of modified rules and a list of (possible) new rules from the original rules
    |> List.fold (fun (prevRules, newRules) rule -> 
        let modRule, newRule =
            substituteAllRules prevRules rule
            |> replaceDirectLeftRecursiveRule2
        (modRule :: prevRules, newRule :: newRules))

    // Reverse lists so that they are in the original order, since they are built up "backward" above
    >> (fun (a, b) -> (List.rev a, List.rev b))

// Same as removeLeftRecursion, but uses original form of rules don't are found to not be 
// recursive.
let removeLeftRecursion2 rules =
    removeLeftRecursionSplit rules
    ||> List.zip3 rules
    |> List.collect (function
        | rule, modRule, Some newRule -> [modRule; newRule]
        | rule, _, None -> [rule])

let asn1GrammarFile = (__SOURCE_DIRECTORY__ + "\\..\\grammar.txt")

let grammarRuleReferences grammar =
    let rec prodRefs prod =
        alternates prod
        |> List.collect (fun (Sequence s) -> s)
        |> List.collect (function
            | Reference r -> [r]
            | Expression e -> prodRefs e
            | _ -> [])

    grammar
    |> List.collect (snd >> prodRefs)
    |> List.distinct

let unresolvedRuleReferences grammar =
    grammarRuleReferences grammar |> List.filter (fun r -> tryFindRule r grammar |> Option.isNone)

let parseBnfString s =
    runParserOnString grammar () "string" s
    |> function Success (ast, _, _) -> Some ast | _ -> None

let parseBnfFile file =
    runParserOnFile grammar () file (System.Text.UTF8Encoding ())
    |> function Success (ast, _, _) -> Some ast | _ -> None

parseBnfFile asn1GrammarFile
|> Option.map removeLeftRecursion
|> Option.map prettyGrammar
|> System.Console.WriteLine

parseBnfFile asn1GrammarFile
|> Option.map removeLeftRecursion2
|> Option.map prettyGrammar
|> System.Console.WriteLine

// Verify that removeLeftRecursion2 creates a grammar that removeLeftRecursion thinks is non 
// recursive
parseBnfFile asn1GrammarFile
|> Option.map removeLeftRecursion2
|> Option.map removeLeftRecursion
|> Option.map List.length

// i=0: Do nothing, since Value rule is not directly recursive
// i=1, j=0: Value rule is substituted into PrefixedValue rule to get "PrefixedValue ::= BitStringValue | PrefixedValue"
// i=1: Remove directly recursive 2nd alternate of PrefixedValue rule to get "PrefixedValue ::= BitStringValue"

// AST types to use:
// Records - sequences with more than one non-constant element
// Unions - rules with two or more distinct types among it's alternates
//  - Optional: could drop empty alternates from rules and encode as an alternate or option type where ever it's used
//  - missing rules should be assumed to have unique types?
// NonEmptyList - has the form A ::= B C, where C matches for a list of the same type, "C ::= ... B C | empty"
// List - has the form "A ::= ... B A | empty
// string primitives, if specified?

// Should substitutions be pulled back out after the left recursion removal???  This probably 
// allows for some more descriptive names in many cases... but maybe it is ambiguous?
// Or is there a way we can save the original rules and use them in cases where no 
// recursion removal was needed?

// Testing for cycles- looks like we just need to prune empty/unused afterward?  It's not quite 
// clear, but I think that the algorithm described in 
// https://en.wikipedia.org/wiki/Left_recursion includes a cycle-removal part, in the "iterate 
// until grammar is unchanged" comment.  Other examples of this algorithm don't include this, and 
// instead just iterate over the rules prior to "Ai".  I think the difference is the cycle 
// detection.
parseBnfString """
A ::= B x
B ::= C x
C ::= A x
"""
|> Option.map removeLeftRecursion

module List =
    let assoc item list =
        List.find (fst >> (=) item) list |> snd

    let tryAssoc item list =
        List.tryFind (fst >> (=) item) list |> Option.map snd

// Depth-First-Search
let dfs graph visited start_node = 
  let rec explore path (visited, cycles) node = 
    if List.contains node path    then visited, node :: cycles else
    if List.contains node visited then visited, cycles else     
      let new_path = node :: path in 
      let edges    = List.assoc node graph in
      let visited, cycles  = List.fold (explore new_path) (visited, cycles) edges in
      node :: visited, cycles
  in explore [] visited start_node

dfs [1, [2; 3]; 2, [2]; 3, []] ([], []) 2
dfs [1, [2; 3]; 2, [1]; 3, []] ([], []) 1
dfs [1, [2; 3]; 2, [1]; 3, [1; 2; 3]] ([], []) 1

let toposort graph = 
  List.fold (fun visited (node,_) -> dfs graph visited node) ([], []) graph

toposort [1, [2; 3]; 2, []; 3, []]

toposort [1, [2; 3]; 2, [3; 4]; 3, [4;5]; 4, [5]; 5, []]
toposort [1, []; 2, [1]; 3, [1;2]; 4, [2;3]; 5, [3;4]]
toposort [1, [2; 3]; 2, [3; 4]; 3, [4;5]; 4, [5]; 5, [2; 3]]

let rec ruleReferences rule =
    alternates rule
    |> List.collect (fun (Sequence s) -> s)
    |> List.collect (function
        | Reference r -> [r]
        | Expression e -> ruleReferences e
        | _ -> [])

let ruleDependencyGraph grammar =
    grammar
    |> List.map (fun rule -> 
        let (name, prod) = rule
        rule,
        ruleReferences prod
        |> List.distinct
        |> List.map (fun n -> List.tryFind (fun (n, _) -> name = n) grammar)
        |> List.choose id)

let declarationOrder grammar = 
    grammar 
    |> ruleDependencyGraph 
    |> toposort 
    |> Tuple.map List.distinct

parseBnfFile asn1GrammarFile
|> Option.map removeLeftRecursion2
|> Option.map declarationOrder
|> Option.map (Tuple.map prettyGrammar)

parseBnfString """
ItemSpec ::= typereference | ItemId "." ComponentId
ComponentId ::= identifier | number | "*"
ItemId ::= ItemSpec
"""
|> Option.map removeLeftRecursion
|> Option.map prettyGrammar

parseBnfFile asn1GrammarFile
|> Option.map prettyGrammar

parseBnfFile asn1GrammarFile
|> Option.map removeLeftRecursion2
|> Option.map groupAlternates
|> Option.map prettyGrammar

// Mapping rules to AST's.  Basic idea is to create a set of type descriptions (records, unions, 
// etc) and a mapping that indicates what AST type each rule returns.
//
// Grammar -> Map<string, TypeDef>
//
// Or maybe start with an initial mapping?
//
// Map<string, TypeDef> -> Grammar -> Map<string, TypeDef>
//
// But, unless the algorithm assumes type names based on rule names (and doesn't need to perform 
// lookups as it is generating type defs), it will need to handle recursively-defined rules.
// Perhaps this is as simple as using toposort to separate the recursive ones out to special 
// handling and process the non-recursive ones in dependency order.

// How to handle alternates
//  - If all the alternates are composed of constants only there is no associated AST type
//  - If composed of only single rules with unique types, it is simply a union type
//  - If all alternates are sequences with only one non-constant rule, it is still a union type 
//    (constant rules don't map to any AST type)
//  - If alternates all have a common single-rule prefix (ignoring constants), we can introduce a 
//    new rule to factor out this prefix, and process the remainder of the sequences recursively.
//  - For all the rules above, we have to recursively resolve rule references and use the 
//    resulting type, not just the top-level term type.  For example, a rule that is just defined 
//    as a constant should be treated as a constant, not a rule reference.  Perhaps when we 
//    encounter a rule reference that is among the rules that we are trying to resolve currently 
//    (i.e., a cycle), we just assume it to have a distinct type so that the recursion can end.

type Field = string * TypeRef

and Composite = {
    fields: Field list
    }

and TypeDef =
    | RecordDef of Field list
    | UnionDef of Field list
    | OptionalDef of TypeRef
    | ListDef of TypeRef
    | List1Def of TypeRef
    | EmptyDef

and TypeRef =
    | TypeReference of string
    | TypeDefinition of TypeDef

let isConst = function 
    | Constant _ -> true
    | _ -> false

// Signature of functions that resolve rules to types
// Map<string * AlternateExpression> -> Map<string * 'r> -> (Map<string * AlternateExpression * Map<string * 'r>)
// This is just a State monad where the state is the pair of maps...

module State =
    let get s = s, s

    let set s = (), s

    let bind f m s = 
        let (a, s') = m s
        (f a) s'

    let retn a = (fun s -> (a, s))

    let map f m s = 
        let (a, s') =  m s
        f a, s'

    let listMap f list (s: 'state) =
        List.mapFold (fun s e -> f e s) s list

    type StateBuilder() =
        member x.Bind(m, f) = bind f m
        member x.Return(a) = retn a
        member x.ReturnFrom(m) = fun s -> m s
        member x.Zero() = ()

    let state = StateBuilder()

(* 
Pruning rules with empty AST's

Examples:

A ::= B C D 
let A = pipe3 B C D (fun b c d -> { A.B = b; A.C = c; A.D = d })
type A = { B: B; C: C; D: D }

A ::= B | C | D
let A = (B |>> A.B) <|> (C |>> A.C) <|> (D |>> A.D)
type A = B of B | C of C | D of D

A ::= B C D ; B and D are empty
let A = C
type A = C

A ::= B | C | D ; B and D are empty
let A = (B |>> A.B) <|> (C |>> A.C) <|> (D |>> A.D)
type A = B of B | C of C | D of D

Interestingly, in the last two examples, only the sequence parser was affected.  But we said that 
a grammar with no non-empty terminals should result in only empty ASTs!  This doesn't seem to 
agree with our union experiment...  Maybe this is succintly described by saying that when a 
sequence parser completes, it only indicates whether all terms parsed or not, whereas when an 
alternate parser returns success, it also has information about which alternate succeeded, i.e., 
it has more information in the success path than the sequence parser (although conversely, the 
sequence parser has more information in the failure path!).  So maybe we need to reformulate this 
again and say that specifying no non-empty terminals results in a bunch of union AST's.

So in summary:
Constant -> empty AST
Reference r -> empty or reference <-- have to avoid cycles
Expression e -> recurse e
Optional e -> Option of recurse e
... similar for many, many1, etc...
Sequence terms -> empty if all terms empty, same as term if one non-empty term, record of non-empty terms, otherwise
Alternate seqs -> same as seq if one seq, union of *all* seqs, otherwise

How to decide whether a recursive (directly or indirectly) is empty?

A ::= A ; empty
A ::= B A ; empty if B is empty, otherwise (impossible) record with fields B and A
A ::= B C A ; same as above, but depends on both B and C
A ::= B A C ; same, just different field order
A ::= B | A ; tempting to consider empty if B is empty, but we've already said above that all 
              alternations are non-empty.  This one is also interesting in that it is basically
              equivalent to A ::= B, since that is all it will match.
A ::= B C; B ::= A C; can tell they should be empty if C is empty, but what is the algorithm to determine it?
  It's maybe useful to realize that such a grammar is non-terminating and thus not very useful.  
  I think when we are trying to determine whether a sequence is empty, we defer deciding on any 
  terminals which are found to be part of a cycle.  Then, all rules in the cycle are empty only 
  if every element of those rules outsid the cycle are empty.  In practice, I think this can only 
  happen with groups of cyclic sequences, since as soon as you encounter an alternate everything 
  is non-empty.

An example:
A ::= C B
B ::= C A

Start with evaluating rule "A".  Look at C, if non-empty, then stop and consider A non-empty.  
Otherwise, evaluate B, passing the set [A] as the "cycle check".  Evaluate B by looking at C (it 
was found to be empty- maybe this is cached).  Then evaluate A- since it is found in the cycle, 
return a value to indicate that "B" is part of the cycle.  The function that was evaluating A now 
knows that the cycle is [A, B].  Since all part of A are either empty or in the cycle, A is 
marked empty.

So, in general, each sequence rule is processed by:
1. add it to the cycle check
2. evaluate each term as either "empty", "non=empty", or "cycle".
3. If any are non-empty, return non-empty.  Otherwise if all are empty, return empty.  Otherwise return "cycle".
4. (Only at the top-level) If none of the terms a non-empty (either empty or "cycle"), mark all rules in the cycle as empty.

There are two levels to the algorithm above- the top-level iterates over the rules in the 
grammar, resetting the cycle-check each time.  At each iteration, one or more rules might be 
marked empty or non-empty.  The inner level only adds rules to the cycle check, and doesn't mark 
any rules that are found to be in the cycle.

Now, we need to extend this algorithm to handle non-sequence rules, too.  Start with an empty map 
of "marked" rules, and iterate over the rules in the grammar:

Outer rule procedure
1. Set cycle set to just the current rule
2. Run inner rule procedure
3. If "cycle", mark as empty

Inner rule procedure
1. If already determined (to be empty or non-empty) return
2. If in cycle, return "cycle"
3. Add the rule to the cycle set
4. Run production procedure
5. If empty of non-empty, mark as such and return

Production procedure
1. If zero alternates, throw exception
2. If more than 1 alternate, return non-empty
3. If 1 alternate, call term procedure for all terms.  If any are non-empty, return non-empty.  
   If all are empty, return empty.  Otherwise, return "cycle".

Term procedure
1. If constant or empty, return empty
2. If rule reference and rule is defined in the grammar, return inner rule procedure.  If undefined, ???
3. If optional (or other higher-order type), return production procedure 

*)
type AstResult = EmptyAst | NonEmptyAst | Cycle

let isEmpty = function EmptyAst -> true | _ -> false

let isNonEmpty = function NonEmptyAst -> true | _ -> false

let rec findNonEmptyTerm grammar (map, cycle) term =
    match term with
    | Constant _ | ExpressionTerm.Empty -> EmptyAst, (map, cycle)
    | Reference r ->
        match List.tryAssoc r grammar with
        | Some prod -> findNonEmptyRule grammar (map, cycle) (r, prod)
        | None -> NonEmptyAst, (map, cycle)
    | Expression e 
    | Optional e
    | ZeroOrMore e
    | OneOrMore e -> findNonEmptyProduction grammar (map, cycle) e

and findNonEmptyProduction grammar (map, cycle) (Alternate sequences) =
    match sequences with
    | [] -> failwith "Unexpected empty alternate rule"
    | [Sequence terms] -> 
        let (results, (map', cycle')) = 
            List.mapFold (findNonEmptyTerm grammar) (map, cycle) terms
        if List.exists isNonEmpty results then NonEmptyAst, (map', cycle')
        else if List.forall isEmpty results then EmptyAst, (map', cycle')
        else Cycle, (map', cycle')
    | _ -> NonEmptyAst, (map, cycle)

and findNonEmptyRule grammar (map, cycle) (name, prod) =
    match Map.tryFind name map with
    | Some r -> r, (map, cycle)
    | None ->
        if Set.contains name cycle then Cycle, (map, cycle)
        else
            match findNonEmptyProduction grammar (map, Set.add name cycle) prod with
            | EmptyAst, (map', cycle') -> EmptyAst, (Map.add name EmptyAst map', cycle')
            | NonEmptyAst, (map', cycle') -> NonEmptyAst, (Map.add name NonEmptyAst map', cycle')
            | _ as c -> c

let findNonEmptyRules grammar =
    grammar |>
    List.fold (fun map rule -> 
        let name = fst rule
        match findNonEmptyRule grammar (map, Set.empty) rule with
        | Cycle, (map', _) -> Map.add name EmptyAst map'
        | _, (map', _) -> map')
        Map.empty

parseBnfString """
A ::= B C
A1 ::= B | C
A2 ::= "asdf" "qwer"
A3 ::= "asdf" | A3 | empty
A4 ::= "qwer" A4
A5 ::= "a" | "b"
A6 ::= empty
A7 ::= A6 A2
"""
|> Option.map findNonEmptyRules

(*

OK, put all this grammar reducing stuff aside, and take a simple aproach of generating an AST/parser for *every* rule

Sometimes, we need to create "local" type definitions, e.g., A ::= B C | D.  In this case, A is a 
union type with two cases.  The first case is a record type with two fields and the second is 
just whatever type is associated with rule "D".  One way to model this effectively is to allow for
types to carry locally-scoped types with them.

Should we make a new set of types to model parser combinator expressions (including AST interactions)?

A ::= B C --> let A = pipe2 B C (fun _1 _2 -> { A.B = _1; A.C = _2 })

makeTypeDecl: production -> typedecl
makeCombinator: typedecl -> bnf -> parser

*)

let rec makeAlternateAst (Alternate sequences) =
    match sequences with
    | [] -> failwith "empty alternate"
    | [sequence] -> makeSequenceAst sequence
    | _ -> makeUnionAst sequences

and makeUnionAst sequences =
    let fields = 
        sequences
        |> List.map (fun s -> 
            let def = makeSequenceAst s
            (generateSequenceFieldName def, def))
    
    TypeDefinition <| UnionDef fields

and generateSequenceFieldName (typeRef: TypeRef) =
    match typeRef with
    | TypeReference r -> r
    | TypeDefinition d ->
        match d with
        | EmptyDef -> "empty"
        | RecordDef d -> d |> List.map fst |> String.concat "And"
        | UnionDef d -> d |> List.map fst |> String.concat "Or"

and makeSequenceAst (Sequence terms) =
    match terms with
    | [] -> failwith "empty sequence"
    | [term] -> makeTermAst term
    | _ -> makeRecordAst terms

and makeRecordAst terms =
    let fields = 
        terms
        |> List.map (fun t -> 
            let def = makeTermAst t
            (generateSequenceFieldName def, def))
    
    TypeDefinition <| RecordDef fields

and makeTermAst term =
    match term with
    | Constant c -> TypeReference "string"
    | Reference r -> TypeReference r
    | Empty -> TypeReference "unit"
    | Expression e -> makeAlternateAst e
    | Optional e -> TypeDefinition (OptionalDef (makeAlternateAst e))
    | ZeroOrMore e -> TypeDefinition (ListDef (makeAlternateAst e))
    | OneOrMore e -> TypeDefinition (List1Def (makeAlternateAst e))

let makeGrammarAst grammar =
    List.map (Tuple.mapSnd makeAlternateAst) grammar

// Here we're generating AST types for a grammar. TODO:
//  - Should distinguish between "rule type" references, and other type references, e.g., string, 
//    unit.  In the latter, they can become field names, which is bad.

parseBnfString """
A1 ::= B
A2 ::= B | C
A3 ::= B C
A4 ::= B C | D
A5 ::= B | A5 | empty
"""
|> Option.map makeGrammarAst

(*
let rec makeTermParser term =
    match term with
    | Reference r -> r, ReferenceDef r
    
    | Constant c ->
        sprintf "(pstring \"%s\")" (escapeString c), ReferenceDef "string"
    
    | Empty -> 
        "(preturn ())", EmptyDef
    
    | ZeroOrMore e -> 
        let (parser, ast) = makeAlternateParser e
        sprintf "(many %s)" parser, GenericReferenceDef ("List", [ast])
    
    | OneOrMore e -> 
        let (parser, ast) = makeAlternateParser e
        sprintf "(many1 %s)" parser, GenericReferenceDef ("List1", [ast])
    
    | Optional e -> 
        let (parser, ast) = makeAlternateParser e
        sprintf "(opt %s)" parser, GenericReferenceDef ("Option", [ast])
    
    |  _ -> failwith (sprintf "Expression term not supported: %A\n" term)

and makeRecordDefFields terms =
    terms |> List.map (fun term -> "fieldname", makeTermParser term)

and generateFieldName typeDef =
    match typeDef with
    | ReferenceDef r -> r
    | EmptyDef -> ""
    | GenericReferenceDef (name, typeParams) -> failwith "Not implemented yet"
    | RecordDef _ -> failwith "Not implemented yet"
    | UnionDef _ -> failwith "Not implemented yet"

and makeNonEmptySequenceParser terms =
    let parsers = List.map makeTermParser terms

    let fieldInfo =
        parsers
        |> List.map (fun (parser, def) ->
            (parser, def, generateFieldName def))

    let termParsers = parsers |> List.map fst |> String.concat " "

    let args = 
        [ for i = 1 to List.length fieldInfo do 
            yield sprintf "_%d" i ]

    let formalArgs = String.concat " " args

    let assignments =
        fieldInfo
        |> List.mapi (fun i (parser, def, fieldName) -> 
            sprintf "%s = _%d" fieldName (i+1))
        |> String.concat "; "
        
    let parser = 
        sprintf "pipe%d %s (fun %s -> { %s })"
            parsers.Length termParsers formalArgs assignments

    let def = 
        fieldInfo
        |> List.map (fun (_, def, fieldName) -> fieldName, def)
        |> RecordDef
    
    parser, def

and makeSequenceParser name (Sequence terms) =
    match terms with
    | [] -> "(preturn ())", EmptyDef
    | [term] -> makeTermParser term
    | _ -> makeNonEmptySequenceParser terms

and makeAlternateParser name (Alternate alts) =
    match alts with
    | [] -> "(preturn ())", EmptyDef
    | [alt] -> makeSequenceParser name alt
    | _ -> makeNonEmptyAlternateParser name alts

and generateUnionField typeDef =
    CultureInfo.CurrentCulture.TextInfo.ToTitleCase (generateFieldName typeDef)

and makeNonEmptyAlternateParser name alts =
    let parsers = List.map makeSequenceParser alts

    let fieldInfo = 
        parsers
        |> List.map (fun (parser, def) ->
            (parser, def, generateUnionField def))

    let parser =
        fieldInfo
        |> List.map (fun (parser, def, field) ->
            let mapper = 
                match def with
                | EmptyDef -> ">>%"
                | _ -> "|>>"

            sprintf "(%s %s %s)" parser mapper field)
        |> String.concat " <|> "

    let def = 
        fieldInfo 
        |> List.map (fun (_, def, field) -> field, def)
        |> UnionDef

    parser, def

let makeGrammarParser grammar =
    List.map (fun (a, b) -> makeAlternateParser a b) grammar

parseBnfString """
A ::= B C
A1 ::= B | C
A2 ::= B | A2 | empty
"""
|> Option.map makeGrammarParser
*)