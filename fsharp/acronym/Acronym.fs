module Acronym

let abbreviate (phrase: string) = 
    let words = phrase.ToUpper().Split([| ' '; '-' |])
    words 
    |> Seq.map (fun (x: string) -> x.Replace("_", ""))
    |> Seq.filter (fun (x: string) -> x.Length > 0)
    |> Seq.map (fun (w: string) -> w |> Seq.head)
    |> System.String.Concat