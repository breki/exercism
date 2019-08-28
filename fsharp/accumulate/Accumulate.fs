module Accumulate

let rec accumulate 
    (func: 'a -> 'b) (input: 'a list): 'b list =
    match input with
    | [] -> []
    | [ x ] -> [ func x ]
    | head::tail -> 
        func head :: accumulate func tail 
