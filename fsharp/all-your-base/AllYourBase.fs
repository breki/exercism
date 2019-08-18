module AllYourBase

open System

let rec build (position: int) (value: int) (outputBase: int): int list =
    let positionValue = (int)(pown outputBase position)
    let digit = (int)(value / positionValue)

    let remainingValue = value - (positionValue * digit)
    match position with
    | 0 -> [ digit ]
    | _ ->
        let sublist = build (position - 1) remainingValue outputBase
        List.append [ digit ] sublist

let rebase (digits: int list) (inputBase: int) (outputBase: int) = 
    match digits, inputBase, outputBase with
    | (_, inputBase, _) when inputBase <= 1 -> None
    | (_, _, outputBase) when outputBase <= 1 -> None
    | (digits, _, _) when 
        digits 
        |> List.exists(fun x -> x < 0 || x >= inputBase) 
        -> None
    | (_, _, _) ->
        let value = 
            digits 
            |> List.rev
            |> List.indexed
            |> List.sumBy (fun (i, x) -> pown inputBase i * x)

        match value with
        | 0 -> Some [0]
        | _ -> 
            let log = Math.Log((float)value, (float) outputBase)
            let rounded = Math.Floor(log)
            let highestPosition = (int)rounded

            Some (build highestPosition value outputBase)
