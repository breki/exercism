module Raindrops

let convert (number: int): string = 
    let factors = 
        [
            3, "Pling"
            5, "Plang"
            7, "Plong"
        ]
    
    let concatIfFactor (textSoFar: string) factorPair : string =
        let (factor, factorText) = factorPair
        match number % factor with
        | 0 -> textSoFar + factorText
        | _ -> textSoFar

    let concatenated = 
        factors 
        |> List.fold concatIfFactor "" 

    match concatenated with
    | "" -> number.ToString(System.Globalization.CultureInfo.InvariantCulture)
    | _ -> concatenated
