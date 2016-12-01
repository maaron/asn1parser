
open System.Collections.Generic

// Try to make a better parser library (than fparsec) with following features:
// - Backtracking by default
// - Generalized char input type
// - Generalized fail values
// - Incremental

// Incremental part can utilize coroutines, similar to the "procedural state machine" approach, so start with that.
module Seq =
    let takeUntil predicate (sequence: IEnumerable<'t>) =
        seq {
            use en = sequence.GetEnumerator()
            let mutable stop = false
            while not stop && en.MoveNext() do
                yield en.Current
                stop <- predicate en.Current
        }

module Co =
    type Wait = Waiting | Ready

    type Co<'s, 'a> = Co of Wait * ('s -> R<'s, 'a>)

    and R<'s, 'a> = 
        | Done of 's * 'a
        | Next of 's * Co<'s, 'a>

    let rec bind f p =
        let (Co (wait, cont)) = p
        let step state =
            match cont state with
            | Done (s, a) -> Next (s, f a)
            | Next (s, pnext) -> Next (s, bind f pnext)
        Co (wait, step)

    let (>>=) p f = bind f p

    let retn v = Co (Ready, fun s -> Done (s, v))

    let map f = bind (fun s -> f s |> retn)

    let (<!>) p f = map f p

    type PBuilder() =
        member x.Bind(p: Co<'s, 'a>, f: 'a -> Co<'s, 'a2>): Co<'s, 'a2> = bind f p
        member x.Return(v: 'a): Co<'s, 'a> = retn v
        member x.ReturnFrom(p) = p

    // This function recursively applies the current state until the procedure is either done 
    // or waiting for the next state
    let rec readyStep (r: R<'s, 'a>): R<'s, 'a> =
        match r with
        | Next (sold, Co (Ready, cont)) -> readyStep (cont sold)
        | _ -> r
    
    // Starts a procedure by returning the first result
    let start p s = 
        let firstResult = 
            match p with
            | Co (Ready, cont) -> cont s
            | Co (Waiting, cont) -> Next (s, p)
        readyStep firstResult

    // Steps a procedure along by returning the next result.  It is recursive, as internally it 
    // actually steps until the procedure indicates it is waiting for the next input
    let step (s: 's) (r: R<'s, 'a>): R<'s, 'a> =
        // Unless we're already done, get the next result and immediately process any "ready" 
        // procedures
        match r with
        | Next (_, Co (_, cont)) -> cont s |> readyStep 
        | _ -> r

    // Applies a sequence of events to a result, stopping early if the procedure completes.
    let stepAll events result =
        let next result event = step event result

        let isDone = function
            | Done _ -> true
            | _ -> false

        Seq.scan next result events
        |> Seq.takeUntil isDone

    // Runs a procedure by stepping it for all the events in the sequence.  Note that the 
    // procedure may not yet be complete when the function returns, as it may still be expecting 
    // further events.  In the event that the procedure completes before all the events are 
    // consumed, the function returns early.
    let run event events proc =
        start proc event |> stepAll events

    // A procedure that just returns the current state
    let getState: Co<'s, 's> = 
        Co (Ready, fun s -> Done (s, s))

    // A procedure that waits for and returns the next state
    let getNextState: Co<'s, 's> =
        Co (Waiting, fun s -> Done (s, s))
    
    // A procedure that applies a function to modify the state
    let setState f =
        let cont s = let snew = f s in Done (snew, snew)
        Co (Ready, cont)
    
    let coroutine = PBuilder()

module Input =
    type Position = int

    type Input<'e> = Position * InputNext<'e>
    
    and InputNext<'e> = 
        | End
        | Remainder of 'e * (unit -> Input<'e>)

    let next = function
        | (pos, End) -> (pos, End)
        | (pos, Remainder (_, getNext)) -> getNext ()

module Backtrack =
    open Input
    type BacktrackInput<'c> = {
        // buffer at the head
        current: Input<'c>

        // subsequent buffers
        buffers: Input<'c> list
        }

    let rec skipEmptyBuffers backtrack =
        match backtrack with
        | { current = (_, End); buffers = (pos, End) :: tail } -> skipEmptyBuffers { current = (pos, End); buffers = tail }
        | { current = (_, End); buffers = head :: tail } -> { current = head; buffers = tail }
        | _ -> backtrack
    
    let advance backtrack =
        match backtrack with
        | { current = (_, End) } -> skipEmptyBuffers backtrack
        | { current = (_, Remainder (_, getNext)) } -> skipEmptyBuffers { backtrack with current = getNext () }
    
    let startBacktrack buffers =
        match buffers with
        | head :: tail -> { buffers = tail; current = head }
        | [] -> { buffers = []; current = (0, End) }

    let backtrackInput backtrack =
        let rec backtrackInputRec backtrack pos =
            let emptySkipped = skipEmptyBuffers backtrack 
            match emptySkipped with
            | { current = (_, End) } -> (pos, End)

            | { current = (_, Remainder (c, _)) } -> 
                let getNext () = backtrackInputRec (advance emptySkipped) (pos + 1)
                (pos, Remainder (c, getNext))

        backtrackInputRec backtrack 0

    let backtrack buffers = startBacktrack buffers |> backtrackInput

    (* Appends a new buffer to the backtrack buffer list *)
    let add buffer backtrack = 
        { backtrack with buffers = List.append backtrack.buffers [buffer] }
        |> skipEmptyBuffers 

module ParserState =
    open Input

    type ParserState<'c> = {
        current: 'c
        backtrack: ('c array) list
        depth: int
        pos: Position
        stack: Position list
        }

    let discardUnused pstate =
        let smallest = min (List.min pstate.stack) pstate.pos
    
    let pushMark pstate =
        { pstate with stack = pstate.pos :: pstate.stack }

    let popMark pstate =
        match pstate.stack with
        | [] -> pstate
        | posHead :: posTail ->
            discardUnused { pstate with 
                stack = posTail 
                pos = posHead }

module Parser =
    open Input
    open ParserState

    type Parser<'c, 'm, 'f> = Co.Co<ParserState<'c>, Result<'c, 'm, 'f>>

    and Result<'c, 'm, 'f> =
        | Match of 'm * Input<'c>
        | Fail of 'f
    
module List =
    open Input

    let toInput l =
        let rec toInputRec pos = function
            | [] -> pos, End
            | head :: tail -> pos, Remainder (head, fun () -> toInputRec (pos + 1) tail)
        toInputRec 0 l

open Parser
open Input
open Backtrack

{ 
    current = List.toInput [1; 2; 3]; 
    buffers = [List.toInput [4; 5; 6]]
}
|> Backtrack.advance
|> Backtrack.advance
|> Backtrack.advance
|> Backtrack.advance
|> Backtrack.advance
|> Backtrack.advance
|> Backtrack.advance

backtrack [List.toInput [1; 2; 3]; List.toInput [4; 5; 6]]
|> next 
|> next
|> next
|> next
|> next
|> next
|> next
|> next