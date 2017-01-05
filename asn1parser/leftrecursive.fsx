
// Study parser combinators that support left-recursion, similar to ScanRat

// First, try without memoization

type Rule = string

type Input<'c, 's> =
    | End
    | Next of 'c * 's * (unit -> Input<'c, 's>)

type Result<'c, 's, 'v> =
    | Success of Input<'c, 's> * 'v
    | Failure

type ErasedResult<'c, 's> = Result<'c, 's, obj>

type MemoState<'c> = {
    results: Map<Rule, ErasedResult<'c, MemoState<'c>>>
    }
