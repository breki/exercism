module Isogram

open System;

let isIsogram (str: string) =
    let alphabet = [| 'a' .. 'z' |] |> Set.ofArray
    let usedChars = str |> Seq.map Char.ToLower |> Set.ofSeq
    let usedLetters = usedChars |> Set.intersect alphabet
    
    let stringOnlyLetters =
        str
        |> Seq.map Char.ToLower
        |> Seq.filter alphabet.Contains
    
    Seq.length stringOnlyLetters = Set.count usedLetters
