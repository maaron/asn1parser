
module String =
    let getc (s: string) i = s.[i]

module Input =

    type Input<'c> = 
        | End
        | Element of 'c * (unit -> Input<'c>)
    
    let next = function
        | End -> End
        | Element (c, next) -> next ()

    let fromStr str =
        let rec fromStrIndex str idx =
            match idx >= String.length str with
            | true -> End
            | false -> Element (String.getc str idx, fun () -> fromStrIndex str (idx + 1))
        fromStrIndex str 0

    let fromList list =
        let rec fromListRec list =
            match list with
            | [] -> End
            | head :: tail -> Element(head, fun () -> fromListRec tail)
        fromListRec list

    type IndexedInput<'c> = Input<'c * int>

    let mapFold f init input =
        let rec unfoldRec input state =
            match input with
            | End -> End
            | Element (c, next) -> 
                match f state c with
                | None -> End
                | Some (c2, state2) -> 
                    Element (c2, fun () -> unfoldRec (next ()) state2)
        unfoldRec input init

    let map f = 
        let fold _ b = Some (f b, ())
        mapFold fold ()

    let index input = 
        let fold i c = Some ((c, i), i + 1)
        mapFold fold 0 input
    
    let toSeq input =
        let unfolder input =
            match input with
            | End -> None
            | Element (c, next) -> Some (c, next ())
        Seq.unfold unfolder input

    let toInputSeq input =
        let unfolder = function
            | End -> None
            | Element (c, next) -> let n = next () in Some (n, n)
        Seq.unfold unfolder input
    
    let skipWhile predicate input =
        input 
        |> toInputSeq 
        |> Seq.skipWhile (function 
            | Element (c, next) when predicate c -> true 
            | _ -> false)
        |> Seq.head

    let filter predicate input =
        let rec filteredRec input =
            match skipWhile (predicate >> not) input with
            | End -> End
            | Element (c, next) -> Element (c, fun () -> filteredRec (next ()))
        filteredRec input

    let duplicate input =
        let fold i c = Some ((c, i), next i)
        mapFold fold input input

    let unduplicate = function
        | End -> End
        | Element ((c, parent), next) -> parent

    let zip input1 input2 =
        mapFold (fun i2 c1 -> 
            match i2 with
            | Element (c2, _) -> Some ((c1, c2), next i2)
            | _ -> None) input2 input1

    let lift combinator input =
        input 
        |> map fst 
        |> combinator
        |> zip input
        |> map (fun ((a, c), b) -> b, c)

    let filterCopy predicate input = filter (fst >> predicate) input

    let mapCopy f = map (fun (a, b) -> (f a, b))

    (*
    let asdf (combinator: Input<'a> -> Input<'b>) (input: Input<'a * Input<'c>>): Input<'b * Input<'c>> =
        let rec asdfRec input =
            match input with
            | End -> End
            | Element ((c, parent), next)

    // lift : (Input<'a> -> Input<'a>) -> Input<'a, Input<'a>> -> Input<'a, Input<'a>>
    let lift combinator input =
        let rec liftRec input =
            match input with
            | End -> End
            | Element ((c, parent), next) -> Element (combinator parent |> duplicate, fun () -> liftRec (next ()))
        liftRec input
*)
open Input

"abcdefghijklmnopqrstuvwxyz" |> fromStr |> index
|> next |> next

[9; 8; 7; 6; 5; 4; 3; 2; 1] |> fromList |> next |> index |> next |> next

"abcdefghijklmnopqrstuvwxyz" |> fromStr |> duplicate |> next |> next

let predicate = ((=) 0)

[0; 0; 1; 2; 3; 4; 5] |> fromList 
|> skipWhile ((=) 0)

[0; 0; 0; 0; 1; 2; 3] |> fromList |> skipWhile ((=) 0)

[9; 8; 7; 6; 5; 4; 3; 2; 1] |> fromList 
|> filter (fun i -> i % 2 = 0)
|> next
|> next
|> next
|> next
|> next

"abcdefghijklmnopqrstuvwxyz" |> fromStr |> duplicate |> next |> next |> unduplicate

[9; 8; 7; 6; 5; 4; 3; 2; 1] |> fromList 
|> duplicate 
|> filterCopy (fun i -> i % 2 = 0) 
|> next 
|> unduplicate 
|> next

[9; 8; 7; 6; 5; 4; 3; 2; 1] |> fromList 
|> duplicate 
|> (filter (fun i -> i % 2 = 0) |> lift)
|> next 
|> unduplicate 
|> next

[4; 3; 2; 1] |> fromList |> map ((*) 2)
|> next
|> next
|> next
|> next
|> next