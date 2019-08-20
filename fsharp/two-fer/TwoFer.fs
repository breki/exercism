module TwoFer

let twoFer (input: string option): string = 
    match input with
        | None -> "you"
        | Some name -> name
    |> sprintf "One for %s, one for me."
