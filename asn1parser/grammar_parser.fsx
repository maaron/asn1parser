﻿
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

module List =
    let assoc item list =
        List.find (fst >> (=) item) list |> snd

    let tryAssoc item list =
        List.tryFind (fst >> (=) item) list |> Option.map snd

    let rec tryRemovePrefix prefix list =
        match prefix, list with
        | [], [] -> Some []
        | ph :: pt, lh :: lt when ph = lh -> tryRemovePrefix pt lt
        | [], l -> Some l
        | p, [] -> None
        | _ -> None

    let rec tryRemoveSuffix suffix list =
        tryRemovePrefix (List.rev suffix) (List.rev list)
        |> Option.map List.rev

    let partition2 f list =
        let folder (ll, rl) e =
            match f e with
            | Choice1Of2 l -> l :: ll, rl
            | Choice2Of2 r -> ll, r :: rl
        let (ll, rl) = List.fold folder ([],[]) list
        (List.rev ll, List.rev rl)

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
    | ZeroOrMore (Alternate [Sequence [term]]) -> prettyTerm term + "*"
    | ZeroOrMore e -> "(" + prettyProduction e + ")*"
    | OneOrMore (Alternate [Sequence [term]]) -> prettyTerm term + "+"
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

let rec prettyProductionIndent (Alternate alts) =
    System.String.Join ("\n    | ", alts |> List.map prettySequenceIndent)

and prettyTermIndent = function
    | Reference r -> r
    | Constant c -> "\"" + escapeString c + "\""
    | Empty -> "empty"
    | Expression e -> "(" + prettyProductionIndent e + ")"
    | ZeroOrMore (Alternate [Sequence [term]]) -> prettyTermIndent term + "*"
    | ZeroOrMore e -> "(" + prettyProductionIndent e + ")*"
    | OneOrMore (Alternate [Sequence [term]]) -> prettyTermIndent term + "+"
    | OneOrMore e -> "(" + prettyProductionIndent e + ")+"
    | Optional (Alternate [Sequence [term]]) -> prettyTermIndent term + "?"
    | Optional e -> "(" + prettyProductionIndent e + ")?"

and prettySequenceIndent (Sequence s) =
    System.String.Join(" ", s |> List.map prettyTermIndent)

and prettyRuleIndent (name, production) =
    name + " ::= " + prettyProductionIndent production

and prettyGrammarIndent g =
    g
    |> Seq.map prettyRuleIndent 
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
let groupAlternates grammar =
    grammar
    |> List.groupBy fst
    |> List.map (fun (name, rules) ->
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
    let alphas, betas = 
        alternates production
        |> List.partition2 (function
            | Sequence (Reference r :: t) when r = name -> Choice1Of2 (Sequence t)
            | Sequence beta -> Choice2Of2 (Sequence beta))
    
    match alphas with
    | [] -> None
    | _ -> Some ((name, production), alphas, betas)

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

let discardEqualityRules2 (name, production) = 
    let remaining, removed = 
        production 
        |> alternates
        |> List.partition (isIdentityRule name >> not)

    (name, Alternate remaining), removed

let addReference reference (Sequence sequence) = Sequence (sequence @ [Reference reference])

let removeFirstReference (Sequence s) = Sequence (List.tail s)

let replaceDirectLeftRecursiveRule = 
    discardEqualityRules >> function
    | MatchDirectLeftRecursive ((name, production), alphas, betas) ->
        let newRule = name + "'"
        let oldRuleAlts = 
            betas |> List.map (addReference newRule)
        
        let newRuleAlts = 
            alphas 
            |> List.map (addReference newRule)
            |> List.append [Sequence [Empty]]

        [ name, Alternate oldRuleAlts
          newRule, Alternate newRuleAlts ]

    | _ as skip -> [skip]

//let discardEqualityRules

// We need to be able to track whether a rule needs to remain modified because an equality rule 
// was removed, or whether it can be swapped back for the original after recursion has been 
// removed.  Therefore, this function returns three values:
// 1. The modified rule found be substituting previous rule references
// 2. A boolean indicating whether the rule must be kept (true = permanent)
// 3. An optional new rule to be added to the grammar
let replaceDirectLeftRecursiveRule2 rule = 
    let (remaining, removed) = discardEqualityRules2 rule

    let permanent = List.isEmpty removed |> not
    
    match remaining with
    | MatchDirectLeftRecursive ((name, production), alphas, betas) ->
        if alphas = betas then
            let alts = 
                alphas
                |> List.map (addReference name)
                |> List.append betas

            (name, Alternate alts), true, None
        
        else
            let newRule = name + "'"
            let oldRuleAlts = 
                betas |> List.map (addReference newRule)
        
            let newRuleAlts = 
                alphas 
                |> List.map (addReference newRule)
                |> List.append [Sequence [Empty]]

            (name, Alternate oldRuleAlts), true, Some (newRule, Alternate newRuleAlts)

    | _ -> remaining, permanent, None

let tryFindRule name =
    List.tryPick (fun (n, p) -> 
        if name = n then Some p else None)

let addTerms terms (Sequence s) = Sequence (List.append s terms)

let substituteRule (name, production) (Alternate sequences) =
    sequences
    |> List.map (fun (Sequence terms) ->
        terms
        |> List.map (function
            | Reference r when r = name -> Expression production
            | _ as t -> t)
        |> Sequence)
    |> Alternate

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
let removeLeftRecursionSplit rules =
    let folder (prevRules, permanents, newRules) rule =
        let modRule, permanent, newRule =
            substituteAllRules prevRules rule
            |> replaceDirectLeftRecursiveRule2
        (modRule :: prevRules, permanent :: permanents, newRule :: newRules)

    List.fold folder ([],[],[]) rules

    |> (fun (a, b, c) -> (List.rev a, List.rev b, List.rev c))
    |||> List.zip3
    |> List.map (function 
        | modified, true, added -> Some modified, added
        | _, false, added -> None, added)

// Same as removeLeftRecursion, but uses original form of rules don't are found to not be 
// recursive.
let removeLeftRecursion2 rules =
    removeLeftRecursionSplit rules
    |> List.zip rules
    |> List.collect (function
        | _,    (Some modified, Some added) -> [modified; added] // [modified |> Tuple.mapSnd (substituteRule added)]
        | _,    (Some modified, None      ) -> [modified]
        | rule, (_,             _         ) -> [rule])

let asn1GrammarFile = (__SOURCE_DIRECTORY__ + "\\..\\grammar.txt")
let asn1GrammarFile2 = (__SOURCE_DIRECTORY__ + "\\..\\grammar2.txt")

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

let inneighbors node graph =
    List.choose (fun (k, v) -> if List.contains node v then Some k else None) graph

let scc graph =
    let rec visit (list, visited) node =
        if Set.contains node visited then (list, visited) else
        let neighbors = 
            match List.tryAssoc node graph with
            | None -> []
            | Some n -> n
        let (list', visited') = List.fold visit (list, Set.add node visited) neighbors
        (node :: list', visited')
    let (l, visited) = 
        graph
        |> List.map fst
        |> List.fold visit ([], Set.empty)

    let rec assign (cycle, assigned) node =
        if Set.contains node assigned then cycle, assigned else
        let ineighbors = inneighbors node graph
        List.fold assign (node :: cycle, Set.add node assigned) ineighbors
    
    List.mapFold (fun assigned node -> assign ([], assigned) node)
        Set.empty l
    |> fst
    |> List.filter (List.isEmpty >> not)

let cycles graph =
    scc graph
    |> List.filter (function 
        | [node] -> 
            match List.tryAssoc node graph with
            | None -> false
            | Some nodes -> List.contains node nodes
        | _ -> true)

scc    [1, []; 2, []; 3, []]
cycles [1, []; 2, []; 3, []]

cycles [1, [2]; 2, [1]; 3, [3]]
cycles [1, [2]; 2, [1]; 3, []]

cycles [1, [1;2;3]; 2, [1;2;3]; 3, [1;2;3]]

cycles [1, [2]; 2, [1; 3]; 3, []]

cycles [1, [2]; 2, [1]; 3, [4]; 4, [3]]

cycles [1, [2;3]; 2, [1]; 3, [4]; 4, [3]]

dfs [1, [2]; 2, [1]; 3, [3]] ([],[]) 1

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

let ruleDeps grammar =
    grammar
    |> List.map (Tuple.mapSnd ruleReferences)

let refersTo name production =
    ruleReferences production
    |> List.contains name

parseBnfFile asn1GrammarFile
|> Option.map ruleDeps
|> Option.map cycles

parseBnfString """
A ::= A B C
"""
|> Option.map ruleDeps
|> Option.map cycles
|> Option.map (List.iter (String.concat "," >> System.Console.WriteLine))

parseBnfString """
ItemSpec ::= typereference | ItemId "." ComponentId
ComponentId ::= identifier | number | "*"
ItemId ::= ItemSpec
"""
|> Option.map removeLeftRecursion
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

and TypeRef =
    | String
    | Unit
    | RuleReference of string
    | OptionalDef of TypeRef
    | ListDef of TypeRef
    | List1Def of TypeRef
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

let rec generateFieldName (Alternate alts) =
    alts |> List.map generateSequenceFieldName |> String.concat "Or"

and generateSequenceFieldName (Sequence terms) =
    terms |> List.map generateTermFieldName |> String.concat "And"

and generateTermFieldName term =
    match term with
    | Constant c -> c
    | Empty -> "empty"
    | Reference r -> r
    | Optional e -> "Optional" + generateFieldName e
    | ZeroOrMore e -> generateFieldName e + "List"
    | OneOrMore e -> generateFieldName e + "List1"
    | Expression e -> generateFieldName e

let rec makeAlternateAst (Alternate sequences) =
    match sequences with
    | [] -> "(preturn ())", Unit
    | [sequence] -> makeSequenceAst sequence
    | _ -> makeUnionAst sequences

and makeUnionAst sequences =
    let fieldNames = sequences |> List.map generateSequenceFieldName
    let parsers = sequences |> List.map makeSequenceAst

    let distinctTypes =
        parsers 
        |> List.map snd
        |> List.distinct

    match distinctTypes with
    | [ast] -> 
        let alternateParser =
            parsers
            |> List.map fst
            |> String.concat " <|> "

        alternateParser, ast

    | _ ->
        let alternateParser =
            List.zip fieldNames parsers
            |> List.map (fun (fieldName, (parser, ast)) ->
                sprintf "(%s |>> %s)" parser fieldName)
            |> String.concat " <|> "

        alternateParser, TypeDefinition <| UnionDef (List.zip fieldNames (List.map snd parsers))

and makeSequenceAst (Sequence terms) =
    match terms with
    | [] -> "(preturn ())", Unit
    | [term] -> makeTermAst term
    | _ -> makeRecordAst terms

and makeRecordAst terms =
    let fieldNames = terms |> List.map generateTermFieldName
    let parsers = terms |> List.map makeTermAst
        
    // Drop fields whose type is unit
    let nonEmptyFields = parsers |> List.filter (function (_, Unit) -> false | _ -> true)
    
    // Replace empty records with unit
    match nonEmptyFields with
    | [] -> 
        let parser = 
            parsers 
            |> List.map fst
            |> String.concat " .>>. "

        parser, Unit

    | [name, ast] -> 
        let parser = 
            parsers 
            |> List.map fst
            |> String.concat " .>>. "

        parser, ast

    | _ -> 
        let termParsers = 
            parsers |> List.map fst
            |> String.concat " "

        let args = 
            [ for i = 1 to List.length parsers do 
                yield sprintf "_%d" i ]

        let formalArgs = String.concat " " args

        let assignments =
            List.zip3 args fieldNames parsers
            |> List.choose (fun (arg, fieldName, (parser, ast)) -> 
                match ast with
                | Unit -> None
                | _ -> Some (sprintf "%s = %s" fieldName arg))
            |> String.concat "; "
        
        let parser = 
            sprintf "pipe%d %s (fun %s -> { %s })"
                parsers.Length termParsers formalArgs assignments
        
        let fields = (List.zip fieldNames (List.map snd parsers))

        parser, TypeDefinition <| RecordDef nonEmptyFields

and makeTermAst term =
    match term with
    | Constant c -> 
        sprintf "(pstring \"%s\" |> ignore)" (escapeString c)
        , Unit
    
    | Reference r -> r, RuleReference r

    | Empty -> "(preturn ())", Unit

    | Expression e -> 
        makeAlternateAst e
        |> Tuple.mapFst (sprintf "(%s)")

    | Optional e -> 
        let (parser, ast) = makeAlternateAst e
        sprintf "(opt %s)" parser, OptionalDef ast

    | ZeroOrMore e -> 
        let (parser, ast) = makeAlternateAst e
        sprintf "(many %s)" parser, ListDef ast
    
    | OneOrMore e -> 
        let (parser, ast) = makeAlternateAst e
        sprintf "(many1 %s)" parser, List1Def ast

let makeGrammarAst grammar =
    List.map (Tuple.mapSnd makeAlternateAst) grammar

// Here we're generating AST types for a grammar. TODO:
//  - Need to identify higher-order types

parseBnfString """
A1 ::= B
A2 ::= B | C
A3 ::= B C
A4 ::= B C | D
A5 ::= B "asdf" | A5 | empty
A6 ::= ( B "qwer" )*
A7 ::= ( B "qwer" )* | B*
A8 ::= "qwer"
A9 ::= ( ( B "asdf" "qwer" ) | B "1234" ) | B
"""
|> Option.map makeGrammarAst

let reduceOptionType production =
    let (nonEmpty, empty) =
        alternates production
        |> List.partition (function Sequence [Empty] -> false | _ -> true)

    match empty with
    | [] -> production
    | _ -> Alternate [Sequence [Optional (Alternate nonEmpty)]]

let reduceOptionTypes grammar =
    grammar |> List.map (Tuple.mapSnd reduceOptionType)

let reduceListType rule =
    match Tuple.mapSnd reduceOptionType rule with
    | name, Alternate [Sequence [Optional (Alternate [Sequence terms])]] as opt ->
        let remaining = 
            terms 
            |> List.tryRemoveSuffix [Reference name]

        match remaining with
        | Some terms -> name, Alternate [Sequence [ZeroOrMore (Alternate [Sequence terms])]]
        | None -> opt
    
    | _ as opt -> opt

let reduceListTypes = List.map reduceListType

parseBnfString """
A ::= B | A | empty
"""
|> Option.map reduceOptionTypes
|> Option.map makeGrammarAst

parseBnfString """
A ::= B A | empty
"""
|> Option.map reduceListTypes
|> Option.map makeGrammarAst

(*

Top-down left-to-right backtracking ambiguities...  There is a problem with parsers that just 
find the first matching alternative, as opposed to trying all of them and returning the "best" 
match.  To make this (maybe) work out of the box, we have to convert rules of the form:

A ::= B | B C

To the following:

A ::= B C | C

But we need a more general rule than that.  I guess we define an ordering based on prefix, 
although to be complete, we'd need to recursively examine sub-rules to find the ultimate prefix.  
Maybe just processing one level is good enough in many cases?  Lets solve the easy problem first
and see how far that takes us...

Interestingly, the example above could also be reduced to this:

A ::= B? C

Also, there seems to be a connection to OneOrMore rules:

A ::= C | C A  ---> C A? ---> C+

This was similar to ZeroOrMore reducing:

A ::= C | A | empty ---> (C A)? ---> C*

More generally,

 A ::= B1 B2 B3 ... BN C | B1 B2 B3 ... BN ---> B1 B2 ... BN C?

*)

let removeDuplicateAlternates grammar =
    grammar
    |> List.map (Tuple.mapSnd (alternates >> List.distinct >> Alternate))

let discardGrammarEqualityRules grammar =
    grammar
    |> List.map discardEqualityRules

let substituteAliases grammar =
    let aliases, remaining =
        grammar
        |> List.partition (function
            | name, Alternate [Sequence [Reference r]] -> true
            | _ -> false)
    
    grammar
    |> List.map (substituteAllRules aliases)

let reducePrefixOptionType production =
    match alternates production with
    | [Sequence l; Sequence r] ->
        match List.tryRemovePrefix l r with
        | Some r' -> Alternate [Sequence (l @ [Optional (Alternate [Sequence r'])])]
        | None ->
            match List.tryRemovePrefix r l with
            | Some l' -> Alternate [Sequence (r @ [Optional (Alternate [Sequence l'])])]
            | None -> production
    | _ -> production

let reducePrefixOptionTypes grammar = grammar |> List.map (Tuple.mapSnd reducePrefixOptionType)

let reduceSuffixOptionType production =
    match alternates production with
    | [Sequence l; Sequence r] ->
        match List.tryRemoveSuffix l r with
        | Some r' -> Alternate [Sequence ([Optional (Alternate [Sequence r'])] @ l)]
        | None ->
            match List.tryRemoveSuffix r l with
            | Some l' -> Alternate [Sequence ([Optional (Alternate [Sequence l'])] @ r)]
            | None -> production
    | _ -> production

let reduceSuffixOptionTypes grammar = grammar |> List.map (Tuple.mapSnd reduceSuffixOptionType)

let reduceList1Type rule =
    match Tuple.mapSnd (reducePrefixOptionType >> reduceSuffixOptionType) rule with
    | name, Alternate [Sequence terms] as opt ->
        let prefix = terms |> List.tryRemoveSuffix [(Optional (Alternate [Sequence [Reference name]]))]

        match prefix with
        | Some [] -> rule
        | Some p -> name, Alternate [Sequence [OneOrMore (Alternate [Sequence p])]]
        | _ -> rule
    
    | _ -> rule

let reduceList1Types = List.map reduceList1Type

parseBnfString """
A ::= B | B C
A1 ::= B C | B
"""
|> Option.map reducePrefixOptionTypes

parseBnfString """
A ::= B | C B
A1 ::= C B | B
"""
|> Option.map reduceSuffixOptionTypes

// reduceList1Types doesn't currently handle the third and fourth rules below.  No reason it can't, but not 
// an issue since we're planning to remove left recursion anyway
parseBnfString """
A1 ::= B | B A1
A2 ::= B A2 | B
A3 ::= A3 B | B
A4 ::= B | A4 B
"""
|> Option.map reduceList1Types

// This could result in 4 List1 types, but the modified (left recusion removal) rules A3 and A4 
// aren't detected as List1 because they depend on added rules A3 and A4, which are detected as 
// List types.
parseBnfString """
A1 ::= B | B A1
A2 ::= B A2 | B
A3 ::= A3 B | B
A4 ::= B | A4 B
"""
|> Option.map removeLeftRecursion
|> Option.map reduceList1Types
|> Option.map reduceListTypes

parseBnfFile asn1GrammarFile
|> Option.map removeLeftRecursion2
|> Option.map reduceListTypes
|> Option.map reduceList1Types
|> Option.map removeDuplicateAlternates
|> Option.map prettyGrammar
|> System.Console.WriteLine

parseBnfFile asn1GrammarFile2
|> Option.map removeLeftRecursion2
|> Option.map reduceListTypes
|> Option.map reduceList1Types
|> Option.map removeDuplicateAlternates
|> Option.map prettyGrammarIndent
|> System.Console.WriteLine

parseBnfFile asn1GrammarFile
|> Option.map removeLeftRecursion2
|> Option.map reduceListTypes
|> Option.map reduceList1Types
|> Option.map removeDuplicateAlternates
|> Option.map makeGrammarAst
|> Option.bind (List.tryAssoc "XMLPrefixedValue")
|> System.Console.WriteLine

parseBnfString """
SymbolsFromModuleList ::= SymbolsFromModule SymbolsFromModuleList1
SymbolsFromModuleList1 ::= SymbolsFromModule SymbolsFromModuleList1 | empty
"""
|> Option.map reduceListTypes

parseBnfString """
A ::= B | B | C | C
"""
|> Option.map removeDuplicateAlternates

parseBnfFile asn1GrammarFile2
|> Option.map discardGrammarEqualityRules
|> Option.map substituteAliases
|> Option.map removeLeftRecursion2
|> Option.map prettyGrammarIndent

(*

One of the big problems with left recursion removal in the ASN.1 grammar is rules of the form:

A ::= B | A ... B

Many of these sort of rules are used to describe delimited lists, e.g.:

ValueList ::= Value | ValueList "," Value

In these cases, if we make this simple transformation:

ValueList ::= Value | Value "," ValueList

We can avoid *huge* amounts of grammar mangling induced by Paull's method.  Or, more generally:

A ::= B | B ... A

*)

let reduceListLikeLeftRecursion rule =
    match rule with
    | name, Alternate [Sequence lterms; Sequence rterms] ->
        let remaining =
            rterms 
            |> List.tryRemovePrefix [Reference name] 
            |> Option.bind (List.tryRemoveSuffix lterms)

        match remaining with
        | None -> rule
        | Some terms -> name, Alternate [Sequence lterms; Sequence (lterms @ terms @ [Reference name])]

    | _ -> rule

let reduceListLikeLeftRecursions grammar =
    grammar |> List.map reduceListLikeLeftRecursion

parseBnfString """
A ::= B | A C D E B
"""
|> Option.map reduceListLikeLeftRecursions

parseBnfString """
C ::= H | I H
B ::= B D | B E | B F | C G
A ::= B | C | A D
"""
|> Option.map removeLeftRecursion2

parseBnfString """
A ::= B | A B
"""
// A  ::= B A'
// A' ::= B A' | empty
|> Option.map (List.map replaceDirectLeftRecursiveRule2)

parseBnfString """
A ::= B | C | A B | A C
"""
// A  ::= B A' | C A'
// A' ::= B A' | C A' | empty
// A  ::= B | C | B A | C A
|> Option.map removeLeftRecursion2

parseBnfFile asn1GrammarFile
|> Option.map removeLeftRecursion2
|> Option.map prettyGrammarIndent

parseBnfFile asn1GrammarFile2
//|> Option.map reduceListLikeLeftRecursions
|> Option.map removeLeftRecursion2
|> Option.map prettyGrammarIndent

parseBnfFile asn1GrammarFile
|> Option.map reduceListLikeLeftRecursions
|> Option.map discardGrammarEqualityRules
//|> Option.map substituteAliases
|> Option.map removeLeftRecursion2
|> Option.map (List.assoc "PrefixedValue")

// The simplification is possible when the set of "alpha" terms is the same as the set of "beta" terms
