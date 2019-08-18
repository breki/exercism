module Proverb

let mainLine word1 word2 =
    Printf.sprintf "For want of a %s the %s was lost." word1 word2    

let lastLine word =
    Printf.sprintf "And all for the want of a %s." word

let recite (input: string list): string list =
    if input.IsEmpty then []
    else
        let pairs = input |> Seq.pairwise
        let mainLines =
            pairs |> Seq.map (fun (word1, word2) -> mainLine word1 word2)
                
        let allLines = Seq.append mainLines [ lastLine input.Head ]
        
        allLines |> Seq.toList
