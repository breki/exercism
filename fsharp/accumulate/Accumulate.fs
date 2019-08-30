module Accumulate

let accumulate func input =
    
    let rec accumulateRecursive func acc = function
        | [] -> acc
        | head::tail -> 
            accumulateRecursive func (func head :: acc ) tail

    accumulateRecursive func [] input |> List.rev
