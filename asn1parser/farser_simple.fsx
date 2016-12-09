
// Simple, backtracking, must have all the input available, no error reporting

// - Need to detect zero-length matches (for many/fold)
// - 

module Input =

    type Input<'c> = 
        | End
        | Element of 'c * (unit -> Input<'c>)
    
    let next = function
        | End -> End
        | Element (c, next) -> next ()

