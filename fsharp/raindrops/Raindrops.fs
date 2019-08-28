module Raindrops

let convert (number: int): string = 
    let factors = 
        [
            3, "Pling"
            5, "Plang"
            7, "Plong"
        ]
    
    let concatIfFactor textSoFar (factor, factorText) =
        match number % factor with
        | 0 -> textSoFar + factorText
        | _ -> textSoFar

    factors 
    |> List.fold concatIfFactor ""
    |> function
        | "" -> number.ToString(System.Globalization.CultureInfo.InvariantCulture)
        | x -> x
