
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

type Production = string * AlternateExpression

type Grammar = Production list

// Bnf grammar
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

open FParsec

let ident = identifier (IdentifierOptions ())

let escapedChar = 
    pchar '\\' >>. anyChar |>> function
        | 'n' -> '\n'
        | 't' -> '\t'
        | 'r' -> '\r'
        | other -> other

let quotedChar = escapedChar <|> (satisfy ((<>) '"'))

let quotedString = between (pchar '"') (pchar '"') (many1Chars quotedChar)

let expression, expressionRef = createParserForwardedToRef()

let term =
    (between (pchar '(' .>> spaces) (pchar ')') expression |>> Expression)
    <|> (ident |>> (fun s ->  if s = "empty" then Empty else Reference s))
    <|> (quotedString |>> Constant)

let sequence =
    (many1 (term .>> spaces) |>> Sequence)

expressionRef.Value <- 
    ((sepBy1 (sequence .>> spaces) (pchar '|' .>> spaces)) |>> Alternate)

let production = ident .>> spaces .>> (pstring "::=") .>> spaces .>>. expression .>> (opt skipNewline)

let grammar = many1 production

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

// Test quoted strings
check "rule ::= \"tok\\\"en\"" <| ["rule", Alternate [Sequence [Constant "tok\"en"]]]
check "rule ::= \"token\"" <| ["rule", Alternate [Sequence [Constant "token"]]]

check "rule ::= anotherrule" <| 
    ["rule", Alternate [
        Sequence [Reference "anotherrule"]]]

check "rule ::= sub1 | sub2" <| 
    ["rule", Alternate [
        Sequence [Reference "sub1"]
        Sequence [Reference "sub2"]]]

let (|MatchReference|_|) alt =
    match alt with
    | Alternate [Sequence [Reference r]] -> Some r
    | _ -> None

let startsWithReference rule def =
    match def with
    | Sequence (Reference r :: _) when r = rule -> true
    | _ -> false

let alternates (Alternate alts) = alts

let (|MatchDirectLeftRecursive|_|) prod =
    let rule, def = prod
    let (recAlts, nonRecAlts) = alternates def |> List.partition (startsWithReference rule)
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
        let newRule = rule + "'"
        let oldRuleAlts = nonRecAlts |> List.map (addReference newRule)
        let newRuleAlts = 
            (recAlts 
                |> List.map removeFirstReference 
                |> List.map (addReference newRule))
             @ [Sequence [Empty]]

        [ (rule, Alternate oldRuleAlts); 
          (newRule, Alternate newRuleAlts) ]

    | _ as skip -> [skip]

let replaceDirectLeftRecursiveRules =
    List.collect replaceDirectLeftRecursiveRule

replaceDirectLeftRecursiveRule ("rule", Alternate [Sequence [Reference "rule"; Reference "rule2"]])
replaceDirectLeftRecursiveRule ("rule", Alternate [Sequence [Reference "rule"; Reference "rule2"]; Sequence [Reference "rule3"; Reference "rule2"]])
(|MatchDirectLeftRecursive|_|) ("rule", Alternate [Sequence [Reference "rule"; Reference "rule2"]])
startsWithReference "rule" (Sequence [Reference "rule"; Reference "rule2"])
alternates (Alternate [Sequence [Reference "rule"; Reference "rule2"]; Sequence [Reference "rule3"; Reference "rule2"]]) |> List.partition (startsWithReference "rule")

let tryFindRule name =
    List.tryFind (fst >> ((=) name))
    >> Option.map snd

let addTerms terms (Sequence s) = Sequence (List.append s terms)

let substituteSequence rules s =
    match s with
    | Sequence (Reference r :: h :: t) -> 
        match tryFindRule r rules with
        | Some prod -> alternates prod |> List.map (addTerms (h :: t))
        | None -> [s]
    | _ -> [s]

let substituteRules rules (name, rule) =
    let alts =
        alternates rule 
        |> List.collect (substituteSequence rules)
        |> Alternate
    name, alts

let substituteRulesUntilNoChange rules rule =
    let mutable ret = rule
    let mutable stop = false
    let mutable count = 0
    while not stop && count < 100 do
        printf "Substituting rules in production %A %d\n" (fst rule) count
        count <- count + 1
        let newRule = substituteRules rules ret
        if newRule <> ret then
            ret <- newRule
        else
            stop <- true
    ret

let removeRuleRecursion (index: int) (rules: Grammar) = Some rules

let removeLeftRecursion rules =
    Seq.unfold removeRuleRecursion rules
    |> List.mapi (fun i rule -> 
        substituteRulesUntilNoChange (rules |> List.take (i)) rule
        |> replaceDirectLeftRecursiveRule)
    |> List.collect id

replaceDirectLeftRecursiveRule ("Expression", Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]])

removeLeftRecursion
    ([
        "Expression", Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]
    ])

removeLeftRecursion
    ([
        "Term", Alternate [Sequence [Reference "Term"; Constant "*"; Reference "Factor"]; Sequence [Reference "Factor"]]
    ])

removeLeftRecursion
    ([
        "Expression", Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]
        "Term", Alternate [Sequence [Reference "Term"; Constant "*"; Reference "Factor"]; Sequence [Reference "Factor"]]
    ])

substituteSequence
    ([
        "Expression", Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]
        "Term", Alternate [Sequence [Reference "Term"; Constant "*"; Reference "Factor"]; Sequence [Reference "Factor"]]
    ])
    (Sequence [Reference "Expression"])

removeLeftRecursion 
    ([
        "Expression", Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]
        "Term", Alternate [Sequence [Reference "Term"; Constant "*"; Reference "Factor"]; Sequence [Reference "Factor"]]
        "Factor", Alternate [Sequence [Constant "("; Reference "Expression"; Constant ")"]; Sequence [Reference "Integer"]]
    ])

removeLeftRecursion 
    ([
        "Expression", Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]
        "Term", Alternate [Sequence [Reference "Term"; Constant "*"; Reference "Factor"]; Sequence [Reference "Factor"]]
        "Factor", Alternate [Sequence [Reference "Expression"]]
    ])

removeLeftRecursion 
    ([
        "Expression", Alternate [Sequence [Reference "Expression"; Constant "-"; Reference "Term"]; Sequence [Reference "Term"]]
        "Term", Alternate [Sequence [Reference "Term"; Constant "*"; Reference "Factor"]; Sequence [Reference "Factor"]]
        "Factor", Alternate [Sequence [Reference "Expression"; Reference "Expression"]]
    ])