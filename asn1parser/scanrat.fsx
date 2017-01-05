
#r "../packages/ScanRat.0.5.0.0/lib/portable-net45+win+wpa81+wp80+MonoAndroid10+xamarinios10+MonoTouch10/ScanRat.dll"

open ScanRat
open ScanRatMatcher
open ScanRatCombinators

let private mkParser name f = Parser(name, fun () -> f)

let private failure index = Failure { ParseFailure.index = index }
let private success index next value = Success { ParseSuccess.index = index; next = next; value = value }
let private success_l index length value = success index (index+length) value

let satisfy f: Parser<char> =
    fun (c: ParserContext) ->
        if c.index + 1 > c.text.Length then failure c.index
        else
            let ch = c.text.[c.index]
            if f ch then
                success_l c.index 1 ch
            else
                failure c.index
    |> mkParser ("satisfy")

let asStr (p: Parser<'a>) =
    fun (c: ParserContext) ->
        let result = p.parse c
        match result with
        | Success s -> 
            printf "c.index = %d, s.index = %d" c.index s.index
            Success { 
                value = c.text.Substring(s.index, s.next - s.index) 
                index = s.index
                next = s.next
            }
        | Failure f -> Failure f
    |> mkParser "as string"

let pMany1 p = p + pMany p --> fun (h, t) -> h :: t

let alpha = satisfy (System.Char.IsLetter)

let ignore p = p --> fun _ -> ()

type Expression = Term list

and Term =
    | Identifier of string
    | Expression of Expression

let identifier = pMany1 alpha |> asStr

let space = satisfy System.Char.IsWhiteSpace

let spaces = pMany space |> ignore

parse identifier "asdf"

let expression = production "expression"

let term = 
    expression --> Expression
    |- identifier .+ spaces --> Identifier

expression.rule <- ~~"(" +. pMany term .+ ~~")" .+ spaces

parse expression "(a (ba bb bc) c)"


