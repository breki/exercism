module PhoneNumber
open System

let justDigits input : string = 
    input |> String.filter (fun c -> Char.IsDigit c)

let areaCodeStart (input: string) =
    input.Chars 0

let exchangeCodeStart (input: string) =
    input.Chars 3

let (|HasLetters|_|) input =
    if input |> String.exists Char.IsLetter then
        Some ()
    else
        None
        
let (|HasPunctuation|_|) input =
    if input |> String.exists (fun c -> "@:!".Contains c) then
        Some ()
    else
        None
        
let (|HasLessThan10Digits|Has10Digits|Has11Digits|HasMoreThan11Digits|)
    (input: string) =
    match (justDigits input).Length with
    | l when l < 10 -> HasLessThan10Digits
    | 10 -> Has10Digits
    | 11 -> Has11Digits
    | _ -> HasMoreThan11Digits

let checkAreaCode (input: string) =
    match areaCodeStart input with
    | '0' -> Error "area code cannot start with zero"
    | '1' -> Error "area code cannot start with one"
    | _ -> Ok input

let checkExchangeCode (input: string) =
    match exchangeCodeStart input with
    | '0' -> Error "exchange code cannot start with zero"
    | '1' -> Error "exchange code cannot start with one"
    | '2' -> Error "exchange code cannot start with two"
    | _ -> Ok input

let parseNumber (input: string)
    : Result<uint64, string> =
    input |> UInt64.Parse |> Ok
        
let clean input =
    match input with
    | HasLetters -> Error "letters not permitted"
    | HasPunctuation -> Error "punctuations not permitted"
    | HasLessThan10Digits -> Error "incorrect number of digits"
    | Has10Digits ->
        (justDigits
            >> checkAreaCode
            >> Result.bind checkExchangeCode
            >> Result.bind parseNumber) input
    | Has11Digits ->
        let cleaned = justDigits input
        match cleaned.Chars 0 with
            | '1' -> cleaned.Substring 1
                     |> checkAreaCode
                     |> Result.bind checkExchangeCode
                     |> Result.bind parseNumber
            | _ -> Error "11 digits must start with 1"
    | HasMoreThan11Digits -> Error "more than 11 digits"
    | _ -> Error "BUG"
