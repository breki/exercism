module CollatzConjecture

let rec step (number: int): int =
    match number with
    | 1 -> 0
    | x when x % 2 = 0 -> step (number / 2) + 1
    | _ -> step (number * 3 + 1) + 1

let steps (number: int): int option =
    match number with
    | x when x <= 0 -> None
    | _ -> Some (step number)