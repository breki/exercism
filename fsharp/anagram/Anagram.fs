module Anagram

let analyzeWord (word: string) =
    let groupedByChars = 
        word 
        |> Seq.groupBy (fun x -> x)
    groupedByChars 
    |> Seq.map (fun (chr, occurrences) -> (chr, Seq.length occurrences))
    |> Seq.sort

let areAnagrams (word1: string) (word2: string) : bool =
    let word1i = word1.ToLower()
    let word2i = word2.ToLower()
    if word1i = word2i then false
    else
        let word1Stats = analyzeWord word1i
        let word2Stats = analyzeWord word2i
        (word1Stats |> Seq.compareWith (Operators.compare)) word2Stats = 0

let findAnagrams (sources: string list) (target: string) = 
    sources |> Seq.filter (fun x -> areAnagrams x target)
    |> Seq.toList