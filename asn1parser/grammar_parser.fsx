
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

let sequence =
    (many1 (term .>> linearSpace) |>> Sequence)

productionRef.Value <- 
    ((sepBy1 (sequence .>> linearSpace) (pchar '|' .>> linearSpace)) |>> Alternate)

let rule = 
    ident .>> linearSpace .>> (pstring "::=") .>> linearSpace .>>. production .>> (opt skipNewline)

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

let removeCycles graph =
    let (sorted, cycles) = toposort graph
    graph 
    |> List.map (Tuple.mapSnd (List.except cycles))

type Field = string * TypeDef

and Record = Field list

and Union = Field list

and TypeDef =
    | RecordDef of Record
    | UnionDef of Union
    | ReferenceDef of string
    | GenericReferenceDef of string * TypeDef list
    | EmptyDef

type NamedType = string * TypeDef

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

open State

type GraphMapState<'a, 'b> = {
    source: Map<string, 'a>
    destination: Map<string, 'b>
    visited: Set<string>
    cycles: Map<string, 'a>
    errors: string list
    }

type ResolveResult =
    | Resolved of TypeDef
    | Unresolved of string
    | Recursive of string

let makeType name def s =
    def, { s with destination = Map.add name def s.destination }

let addError message s =
    (), { s with errors = message :: s.errors }

let visitRule (name, prod) s = 
    (), { s with visited = Set.add name s.visited }

let resolve key f = state {
    let! s = get
    match Map.tryFind key s.destination with
    | Some v -> return Resolved v
    | None ->
        if Set.contains key s.visited then return Recursive key else
        match Map.tryFind key s.source with
        | Some v -> 
            return! f v |> State.map Resolved
        | None -> 
            do! addError <| sprintf "Unresolved type reference %s" key
            return Unresolved key
}

let generateUnionFieldName (Sequence sequence) =
    sequence
    |> List.choose (function
        | Reference r -> Some r
        | _ -> None)
    |> String.concat "And"

let rec generateTermType term s =
    match term with
    | Reference r -> 
        let (def, s') = resolve r generateAlternateType s
        match def with
        | Resolved d -> (r, d), s'
        | Recursive r -> (r, TypeDef.ReferenceDef r), s'
        | Unresolved r -> failwith <| sprintf "Unresolved type for rule %s" r
    | Constant _ | ExpressionTerm.Empty -> ("", EmptyDef), s
    | Expression _ -> failwith "Sub-expressions are not supported"
    | _ -> failwith "Unsupported expression type"

and generateSequenceType (Sequence sequence) state =  
    let (termTypes, s') = State.listMap generateTermType sequence state
    let fields = termTypes |> List.filter (function (_, EmptyDef) -> false | _ -> true)

    match fields with
    | [] -> None, s'
    | [(name, t)] -> Some t, s'
    | _ -> Some (RecordDef fields), s'

and generateAlternateType (Alternate alts) state =
    let (seqTypes, s') = State.listMap generateSequenceType alts state
    
    let fields = 
        List.zip alts seqTypes
        |> List.choose (fun (seq, def) -> 
            match def with
            | Some d -> Some (generateUnionFieldName seq, d)
            | None -> None)

    match fields with
    | [] -> EmptyDef, s'
    | [(name, def)] -> def, s'
    | _ -> UnionDef fields, s'

and generateRuleType (name, production) = state {
    do! visitRule (name, production)
    return!
        generateAlternateType production
        |> State.bind (makeType name)
}

let generateGrammarTypes predefinedTypes grammar =
    let (rules, s) = 
        State.listMap generateRuleType grammar { 
            source = grammar |> Map.ofList
            destination = predefinedTypes
            visited = Set.empty
            cycles = Map.empty
            errors = []}
    
    s.destination

parseBnfString """
A ::= "a constant string"
"""
|> Option.map (fun grammar ->
    generateRuleType grammar.Head { 
        source = grammar |> Map.ofList
        destination = Map.empty
        visited = Set.empty
        cycles = Map.empty
        errors = []})

parseBnfString """
A ::= "a constant string"
"""
|> Option.map (generateGrammarTypes Map.empty)

// The types for rules which aren't defined in the grammar must be provided up front, otherwise 
// an exception is thrown.  An alternative is to just treat them as "external", and assume they 
// have a non-trivial type.
try
    parseBnfString """
A1 ::= B C
"""
    |> Option.map (generateGrammarTypes Map.empty)
with 
    | _ as e ->
        printf "%s\n" e.Message
        None

// Rule A3 below generates a "local" type "BAndC", but doesn't add this to the map.  However, we 
// also need to ensure that such types are unique (e.g., namespacing)
parseBnfString """
A1 ::= B C
A2 ::= B | C
A3 ::= B | B C
"""
|> Option.map (generateGrammarTypes
    (["B", ReferenceDef "BType"; "C", ReferenceDef "CType"] |> Map.ofList))

// Currently, rules such as A4 below result in "copies" of sub-rules (A3 and A2, in this case).  
// I think we should create another "type" (this word is getting too overloaded...) that 
// represents a "referenced" type.  It can have a reference to the actual definition, but this
// referencing should be explicit so the caller knows whether to generate code for the type or
// not.  However, there might be cases where we want to treat the referenced definition and the 
// "referenced type" type as the same.  I suspect this may arise when adding some logic to use 
// higher-level types, such as lists.  The alternative is to leave the copies as is, and 
// instead handle type references in a post-processing step.
parseBnfString """
A1 ::= B C
A2 ::= B | C
A3 ::= B | B C
A4 ::= A3 A2
"""
|> Option.map (generateGrammarTypes
    (["B", ReferenceDef "BType"; "C", ReferenceDef "CType"] |> Map.ofList))

// Recursive rules are currrently generating "Recursive" type defs, which must be further 
// processsed.  The example below could be turned into a "B list" type.
parseBnfString """
A1 ::= B | A1
"""
|> Option.map (generateGrammarTypes
    (["B", ReferenceDef "BType"; "C", ReferenceDef "CType"] |> Map.ofList))

// Sub-expressions are not currently supported (not needed for ASN.1, anyway), and should throw 
// an exception indicating such.
try
    parseBnfString """
A1 ::= B
A1 ::= C
A3 ::= B (B | C) C
"""
    |> Option.map (generateGrammarTypes
        (["B", ReferenceDef "BType"; "C", ReferenceDef "CType"] |> Map.ofList))
with
    | _ as e -> 
        printf "%s\n" e.Message
        None

// Alternative method for simplifying AST types:
// Start with a simple, "everything has a unique type" assumption, and generate a data structure 
// that represents the parsers and their AST types.
// Then, "fold in" all the rules by substituting sub-rules and modifying AST types as neccessary.
// Note that this includes parser maintenance, whereas that hasn't been addressed in the prior 
// method above.  The advantage of this method is that we don't have to explicitly deal with 
// cycles.

// The following represents a similar approach to the one above, but involves extending the BNF 
// AST types to encode higher-level constructs, such as lists, options, etc.  This makes the 
// parser generate functions simpler, and hopefully transforms at the BNF level to introduce 
// higher-level types will be simpler, too.  Also, it has the advantage that we can directly 
// support things like kleene star, '+', or '?' right in the BNF syntax.

// However, there is one drawback in that we have to perform substitutions in order to generate 
// empty definitions (since rule references are assumed to have non-empty AST types).  This makes 
// for a lot of redundancy if we directly generate the substituted productions.

type AlternateParser = AlternateParser of SequenceParser list

and SequenceParser = SequenceParser of ParserDef list

and ParserDef =
    | ReferencedParser of string * TypeDef
    | ConstantParser of string
    | ParserExpression of AlternateParser
    | ZeroOrMoreParser of AlternateParser
    | OneOrMoreParser of AlternateParser
    | OptionalParser of AlternateParser
    | EmptyParser

let rec makeTermParser term =
    match term with
    | ReferencedParser (r, def) -> r, def
    
    | ConstantParser c ->
        sprintf "(pstring \"%s\")" (escapeString c), EmptyDef
    
    | EmptyParser -> 
        "(preturn ())", EmptyDef
    
    | ZeroOrMoreParser e -> 
        let (parser, ast) = makeAlternateParser e
        sprintf "(many %s)" parser, GenericReferenceDef ("List", [ast])
    
    | OneOrMoreParser e -> 
        let (parser, ast) = makeAlternateParser e
        sprintf "(many1 %s)" parser, GenericReferenceDef ("List1", [ast])
    
    | OptionalParser e -> 
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

and makeRecordSequenceParser parsers =
    let fieldInfo =
        parsers
        |> List.map (fun (parser, def) ->
            (parser, def, generateFieldName def))

    let termParsers = parsers |> List.map fst |> String.concat " "

    let args = 
        [ for i = 0 to List.length fieldInfo do 
            yield sprintf "_%d" i ]

    let formalArgs = String.concat " " args

    let assignments =
        fieldInfo
        |> List.mapi (fun i (parser, def, fieldName) -> 
            match def with
            | EmptyDef -> None
            | _ -> Some <| sprintf "%s = _%d" fieldName i)
        |> List.choose id
        |> String.concat "; "
        
    let parser = 
        sprintf "pipe%d %s (fun %s -> { %s })"
            parsers.Length termParsers formalArgs assignments

    let def = 
        fieldInfo
        |> List.map (fun (_, def, fieldName) -> fieldName, def)
        |> RecordDef
    
    parser, def

and makeNonEmptySequenceParser terms =
    let parsers = List.map makeTermParser terms

    let nonEmptyAsts = 
        parsers 
        |> List.map snd
        |> List.filter (function EmptyDef -> false | _ -> true)

    match nonEmptyAsts with
    | [] -> String.concat " .>> " (parsers |> List.map fst), EmptyDef
    | [ast] -> String.concat " .>> " (parsers |> List.map fst), ast
    | _ -> makeRecordSequenceParser parsers

and makeSequenceParser (SequenceParser terms) =
    match terms with
    | [] -> "(preturn ())", EmptyDef
    | [term] -> makeTermParser term
    | _ -> makeNonEmptySequenceParser terms

and makeAlternateParser (AlternateParser alts) =
    match alts with
    | [] -> "(preturn ())", EmptyDef
    | [alt] -> makeSequenceParser alt
    | _ -> makeNonEmptyAlternateParser alts

and generateUnionField typeDef =
    CultureInfo.CurrentCulture.TextInfo.ToTitleCase (generateFieldName typeDef)

and makeNonEmptyAlternateParser alts =
    let parsers = List.map makeSequenceParser alts

    // Check whether any of the child parsers contain a non-empty type def.  This determines 
    // whether or not we generate a union AST type.
    if parsers |> List.exists (function | (p, EmptyDef) -> false | _ -> true) then
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

    else
        parsers |> List.map fst |> String.concat " <|> ", EmptyDef

let makeGrammarParser grammar = 
    let rec makeAlternateParser2 (Alternate alts) = List.map makeSequenceParser2 alts |> AlternateParser

    and makeSequenceParser2 (Sequence terms) = List.map makeTermParser2 terms |> SequenceParser

    and makeTermParser2 = function
        | Reference r -> ReferencedParser (r, EmptyDef)
        | Constant c -> ConstantParser c
        | Empty -> EmptyParser
        | Expression e -> ParserExpression (makeAlternateParser2 e)
        | OneOrMore e -> OneOrMoreParser (makeAlternateParser2 e)
        | ZeroOrMore e -> ZeroOrMoreParser (makeAlternateParser2 e)
        | Optional e -> OptionalParser (makeAlternateParser2 e)

    List.map (Tuple.mapSnd makeAlternateParser2)

parseBnfString """
A ::= "asdf" "qwer"
"""
|> Option.map makeGrammarParser

parseBnfString """
A ::= B C
B ::= "asdf"
C ::= D "qwer"
"""
|> Option.map makeGrammarParser

// Try starting with no AST's and substitute type definitions from the "bottom up"


(* 
A ::= B C ; rec B, C
B ::= C A ; rec C, A
C ::= A B ; rec A, B

A ::= B C   ; rec B, C
B ::= C B C ; rec C, (rec B, C)
C ::= (B C) B ; rec (rec B C), B

A ::= B C   ; rec B, C
B ::= C (B C) ; rec C, (rec B, C)
C ::= ((C B C) C) (C B C) ; rec (rec (rec C B C), C), B

A ::= B A | empty 
C ::= B B

To add a type definition for a rule:
1. Add the rule to the set of non-empty-AST rules
2. Search for all references to the rule and mark them as "referenced type"
3. For each production that changed from empty to non-empty, recurse with changed rule

Specifying a type definition for a non-terminal maybe doesn't make sense?  Otherwise, there would 
be a conflict between the generated type and the statically specified one.  It seems that we 
don't actually need a cycle-aware algorithm for generating type definitions, once it is known 
which terminals have non-empty AST's.  I suppose the cycle-aware part could be in the "front" 
(actively mark non-terminals that are affected by the non-empty terminal AST) or "back" 
(recursively lookup non-terminal emptiness by evaluating it's terms).
*)
type B = int

type A1 = { B: B; A: A }

and A = 
    | A' of A1
    | EmptyA

A' { B = 123; A = EmptyA }
A' { B = 123; A = A' { B = 234; A = EmptyA} }