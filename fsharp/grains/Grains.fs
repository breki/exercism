module Grains

let square (n: int): Result<uint64,string> =
    match n with
        | n when n < 1 || n > 64 -> Error "square must be between 1 and 64"
        | _ -> n - 1 |> pown 2UL |> Ok

let extractValue (res: Result<uint64,string>) : uint64 =
    match res with
        | Ok value -> value
        | Error err -> 0UL

let total: Result<uint64,string> = 
    let results = [| for i in 1 .. 64 -> square i |]
    results |> Seq.sumBy(fun res -> extractValue res) |> Ok
