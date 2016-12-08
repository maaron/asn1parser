

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

    let delayed input =
        let folder prev c = 
            match prev with
            | None -> (c, None), Some input
            | Some p -> let pnext = next p in (c, Some p), Some pnext
            |> Some

        mapFold folder None input

    

module Parser =
    open Input

    type StatefulInput<'c, 's> = Input<'c> * 's

    type Result<'c, 's, 'm, 'f> =
        | Match of 'm * StatefulInput<'c, 's>
        | Fail of 'f

    type Parser<'c, 's, 'm, 'f> = StatefulInput<'c, 's> -> Result<'c, 's, 'm, 'f>

open Input

[1; 2; 3; 4; 5] |> fromList |> delayed
|> next
|> next
|> next