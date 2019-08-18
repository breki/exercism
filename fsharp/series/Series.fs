module Series

let slices (str: string) (length: int) : string list option =
    match length with
        | _ when length > 0 && length <= str.Length ->
            Some [ for i in 0 .. str.Length - length ->
                    str 
                    |> Seq.skip i 
                    |> Seq.take length 
                    |> System.String.Concat ]
        | _ -> None
