module Bob

open System

let stripWhitespace (input: string) = input.Trim()
let hasChar predicate = stripWhitespace >> Seq.exists predicate
let lastStripped = stripWhitespace >> Seq.last

let (|IsEmpty|_|) (input: string) = 
    if (input |> stripWhitespace).Length = 0 then 
        Some IsEmpty
    else 
        None

let (|HasLowCaps|NoLowCaps|) (input: string) =
    let hasLowCaps = 
        input |> hasChar Char.IsLower
    if hasLowCaps then HasLowCaps
    else NoLowCaps

let (|HasLetters|NoLetters|) (input: string) =
    let hasLetters = 
        input |> hasChar Char.IsLetter

    if hasLetters then HasLetters
    else NoLetters

let (|Question|NoQuestion|) input =
    let lastChar = input |> lastStripped
    match lastChar with
        | '?' -> Question
        | _ -> NoQuestion

let response (input: string): string =
    match input with
        | IsEmpty -> "Fine. Be that way!"
        | NoLowCaps & HasLetters & Question 
            -> "Calm down, I know what I'm doing!"
        | NoLowCaps & HasLetters -> "Whoa, chill out!"
        | NoQuestion -> "Whatever."
        | _ -> "Sure."
