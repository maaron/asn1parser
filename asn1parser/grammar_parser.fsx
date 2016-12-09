
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

let grammar = many1 rule

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

let (|MatchReference|_|) alt =
    match alt with
    | Alternate [Sequence [Reference r]] -> Some r
    | _ -> None

let startsWithReference rule def =
    match def with
    | Sequence (Reference r :: _) when r = rule -> true
    | _ -> false

let alternates (Alternate alts) = alts

let (|MatchDirectLeftRecursive|_|) rule =
    let (recAlts, nonRecAlts) = alternates rule.production |> List.partition (startsWithReference rule.name)
    match recAlts with
    | [] -> None
    | _ -> Some (rule, recAlts, nonRecAlts)

let discardEqualityRules = 
    List.filter (function 
        | (rule, MatchReference reference) when rule = reference -> false 
        | _ -> true)

discardEqualityRules ["rule", Alternate [Sequence [Reference "rulea"]]]
discardEqualityRules ["rule", Alternate [Sequence [Reference "rule"]]]

let addReference reference (Sequence sequence) = Sequence (sequence @ [Reference reference])

let removeFirstReference (Sequence s) = Sequence (List.tail s)

let replaceDirectLeftRecursiveRule = function
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

replaceDirectLeftRecursiveRule { name = "Expression"; production = Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]}

removeLeftRecursion
    ([
        { name = "Expression"; production = Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]}
    ])

removeLeftRecursion
    ([
        { name = "Term"; production = Alternate [Sequence [Reference "Term"; Constant "*"; Reference "Factor"]; Sequence [Reference "Factor"]]}
    ])

removeLeftRecursion
    ([
        { name = "Expression"; production = Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]}
        { name = "Term"; production = Alternate [Sequence [Reference "Term"; Constant "*"; Reference "Factor"]; Sequence [Reference "Factor"]]}
    ])

substituteSequence
    ([
        { name = "Expression"; production = Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]}
        { name = "Term"; production = Alternate [Sequence [Reference "Term"; Constant "*"; Reference "Factor"]; Sequence [Reference "Factor"]]}
    ])
    (Sequence [Reference "Expression"])

removeLeftRecursion 
    ([
        { name = "Expression"; production = Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]}
        { name = "Term"; production = Alternate [Sequence [Reference "Term"; Constant "*"; Reference "Factor"]; Sequence [Reference "Factor"]]}
        { name = "Factor"; production = Alternate [Sequence [Constant "("; Reference "Expression"; Constant ")"]; Sequence [Reference "Integer"]]}
    ])

removeLeftRecursion 
    ([
        { name = "Expression"; production = Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]}
        { name = "Term"; production = Alternate [Sequence [Reference "Term"; Constant "*"; Reference "Factor"]; Sequence [Reference "Factor"]]}
        { name = "Factor"; production = Alternate [Sequence [Reference "Expression"]]}
    ])

removeLeftRecursion 
    ([
        { name = "Expression"; production = Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]}
        { name = "Term"; production = Alternate [Sequence [Reference "Term"; Constant "*"; Reference "Factor"]; Sequence [Reference "Factor"]]}
        { name = "Factor"; production = Alternate [Sequence [Reference "Expression"; Reference "Expression"]]}
    ])
    |> prettyGrammar |> System.Console.WriteLine

runParserOnFile grammar () "C:\\Users\\maaron\\Source\\Repos\\asn1parser\\grammar.txt" (System.Text.UTF8Encoding ())
|> function 
    | Failure (e, b, c) -> 
        System.Console.WriteLine e

    | Success (s, _, pos) -> 
        System.Console.WriteLine (s |> prettyGrammar)
        System.Console.WriteLine pos

let asn1nonrec = 
    runParserOnFile 
        (grammar |>> removeLeftRecursion)
        () 
        "C:\\Users\\maaron\\Source\\Repos\\asn1parser\\grammar.txt" 
        (System.Text.UTF8Encoding ())
    |> function Success (ast, _, _) -> ast | Failure (e, _, _) -> failwith e

let ruleReferences grammar =
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
    ruleReferences grammar |> List.filter (fun r -> tryFindRule r grammar |> Option.isNone)

unresolvedRuleReferences asn1nonrec |> List.length
asn1nonrec |> List.length