module Pangram

open System;

let isPangram (input: string): bool =
    let allChars = [ 'a' .. 'z' ] |> Set
    let usedChars =
        Set.ofSeq input
        |> Set.map (Char.ToLower)
    Set.isSubset allChars usedChars
    