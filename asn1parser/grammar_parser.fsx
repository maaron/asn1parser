
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
    | Empty

type Rule = { name: string; production: AlternateExpression }

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
    | Expression e -> prettyProduction e

and prettySequence (Sequence s) =
    System.String.Join(" ", s |> List.map prettyTerm)



and prettyRule r =
    r.name + " ::= " + prettyProduction r.production

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

let sequence =
    (many1 (term .>> linearSpace) |>> Sequence)

productionRef.Value <- 
    ((sepBy1 (sequence .>> linearSpace) (pchar '|' .>> linearSpace)) |>> Alternate)

let rule = 
    ident .>> linearSpace .>> (pstring "::=") .>> linearSpace .>>. production .>> (opt skipNewline)
    |>> (fun (n, p) -> { name = n; production = p})

let alternates (Alternate alts) = alts

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

// Test quoted strings
check "rule ::= \"tok\\\"en\"" <| [{ name = "rule"; production = Alternate [Sequence [Constant "tok\"en"]]}]
check "rule ::= \"token\"" <| [{ name = "rule"; production =Alternate [Sequence [Constant "token"]]}]

check "rule ::= anotherrule" <| 
    [{ name = "rule"; 
       production = Alternate [Sequence [Reference "anotherrule"]]}]

check "rule ::= sub1 | sub2" <| 
    [{
        name = "rule"
        production = 
            Alternate [
                Sequence [Reference "sub1"]
                Sequence [Reference "sub2"]]
    }]

// Combine separate rules with equal names into a single rule with multiple alternates
let groupAlternates =
    List.groupBy (fun r -> r.name)
    >> List.map (fun (n, rules) ->
      { name = n
        production =
            rules
            |> List.collect ((fun r -> r.production) >> alternates) 
            |> Alternate
      })

let startsWithReference rule def =
    match def with
    | Sequence (Reference r :: _) when r = rule -> true
    | _ -> false

let (|MatchDirectLeftRecursive|_|) rule =
    let (recAlts, nonRecAlts) = alternates rule.production |> List.partition (startsWithReference rule.name)
    match recAlts with
    | [] -> None
    | _ -> Some (rule, recAlts, nonRecAlts)

let isIdentityRule name = function
    | Sequence [Reference r] when r = name -> true
    | _ -> false

let discardEqualityRules rule = 
    { rule with 
        production = 
            rule.production 
            |> alternates
            |> List.filter (isIdentityRule rule.name >> not)
            |> Alternate
    }

let addReference reference (Sequence sequence) = Sequence (sequence @ [Reference reference])

let removeFirstReference (Sequence s) = Sequence (List.tail s)

let replaceDirectLeftRecursiveRule = 
    discardEqualityRules >> function
    | MatchDirectLeftRecursive (rule, recAlts, nonRecAlts) ->
        let newRule = rule.name + "'"
        let oldRuleAlts = nonRecAlts |> List.map (addReference newRule)
        let newRuleAlts = 
            (recAlts 
                |> List.map removeFirstReference 
                |> List.map (addReference newRule))
             @ [Sequence [Empty]]

        [ { name = rule.name; production = Alternate oldRuleAlts}
          { name = newRule; production = Alternate newRuleAlts } ]

    | _ as skip -> [skip]

// Returns modified and new rule separately
let replaceDirectLeftRecursiveRule2 = 
    discardEqualityRules >> function
    | MatchDirectLeftRecursive (rule, recAlts, nonRecAlts) ->
        let newRule = rule.name + "'"
        let oldRuleAlts = nonRecAlts |> List.map (addReference newRule)
        let newRuleAlts = 
            (recAlts 
                |> List.map removeFirstReference 
                |> List.map (addReference newRule))
             @ [Sequence [Empty]]

        { name = rule.name; production = Alternate oldRuleAlts},
        Some { name = newRule; production = Alternate newRuleAlts }

    | _ as skip -> skip, None

let tryFindRule name =
    List.tryPick (fun r -> 
        if r.name = name then Some r.production else None)

let addTerms terms (Sequence s) = Sequence (List.append s terms)

let substituteSequence rules s =
    match s with
    | Sequence (Reference r :: t) -> 
        match tryFindRule r rules with
        | Some prod -> alternates prod |> List.map (addTerms t)
        | None -> [s]
    | _ -> [s]

let substituteRules rules rule =
    { rule with 
        production = 
            alternates rule.production
            |> List.collect (substituteSequence rules)
            |> Alternate
    }

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
    |> List.collect (fun r -> prodRefs r.production)
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

parseBnfString
    """
Value ::= BitStringValue | PrefixedValue
PrefixedValue ::= Value
"""
|> Option.map (List.fold (fun prevRules rule -> 
                    substituteAllRules prevRules rule
                    //|> replaceDirectLeftRecursiveRule
                    |> List.singleton
                    |> List.append prevRules) [])
|> Option.map (List.map replaceDirectLeftRecursiveRule)

{name = "PrefixedValue";
      production =
       Alternate
         [Sequence [Reference "BitStringValue"];
          Sequence [Reference "PrefixedValue"]];}
|> replaceDirectLeftRecursiveRule

{name = "PrefixedValue";
      production =
       Alternate
         [Sequence [Reference "BitStringValue"];
          Sequence [Reference "PrefixedValue"]];}
|> (discardEqualityRules >> function
    | MatchDirectLeftRecursive (rule, recAlts, nonRecAlts) ->
        let newRule = rule.name + "'"
        let oldRuleAlts = nonRecAlts |> List.map (addReference newRule)
        let newRuleAlts = 
            (recAlts 
                |> List.map removeFirstReference 
                |> List.map (addReference newRule))
             @ [Sequence [Empty]]

        [ { name = rule.name; production = Alternate oldRuleAlts}
          { name = newRule; production = Alternate newRuleAlts } ]

    | _ as skip -> [skip])

{ name = "PrefixedValue"; production = Alternate [Sequence [Reference "BitStringValue"]; Sequence [Reference "PrefixedValue"]] }
|> discardEqualityRules

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

// Put the rules in "declare before use" order, and mark those which must be recursive
let dfs graph visited start_node = 
  let rec explore path (visited, cycles) node = 
    if List.contains node path    then visited, node :: cycles else
    if List.contains node visited then visited, cycles else     
      let new_path = node :: path in 
      let edges    = List.assoc node graph in
      let visited, cycles  = List.fold (explore new_path) (visited, cycles) edges in
      node :: visited, cycles
  in explore [] visited start_node

let toposort graph = 
  List.fold (fun visited (node,_) -> dfs graph visited node) ([], []) graph

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
    |> List.map (fun r -> (r.name, r.production))
    |> List.map (fun rule -> 
        let (name, prod) = rule
        rule,
        ruleReferences prod
        |> List.distinct
        |> List.map (fun n -> List.tryFind (fun r -> r.name = n) grammar)
        |> List.choose id
        |> List.map (fun r -> (r.name, r.production)))

let declarationOrder grammar = 
    grammar 
    |> ruleDependencyGraph 
    |> toposort 
    |> Tuple.map (
        List.distinct 
        >> List.map (fun (n, p) ->
            { name = n; production = p }))

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

type TypeRef = TypeRef of string * (string option) list

type Field = Field of string * TypeRef

type Record = Record of string * Field list

type Union = Union of string * Field list

type TypeDef =
    | TypeRef of TypeRef
    | Record of Record
    | Union of Union

let isConst = function 
    | Constant _ -> true
    | _ -> false

let generateRecord name sequence = 
    let references =
        sequence |> List.filter (function 
            | Reference _ -> true 
            | _ -> false)

    match references with
    | [Reference r] -> TypeRef (TypeRef (r, None))
    | refs -> Record (name, List.map (fun r -> Field (r, TypeRef r)) refs)

let generateUnion = function
    | Alternate [alt] -> generateRecord alt

let generateDirectTypes grammar =
    grammar
    |> List.map (fun r ->
        match r.production with
        | Alternate [alt] -> generateRecord alt)