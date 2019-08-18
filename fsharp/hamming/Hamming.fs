module Hamming

let distanceChr (strand1: string) (strand2: string) index =
    if strand1.Chars index <> strand2.Chars index then 1
    else 0

let distance (strand1: string) (strand2: string): int option =
    if strand1.Length <> strand2.Length then None
    else
        let charDistances = [ for i in 0 .. strand1.Length - 1 
            -> distanceChr strand1 strand2 i ]
        Some (List.sum charDistances)
