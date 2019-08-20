module ArmstrongNumbers

let rec digitize (number: int): int list =
    let digit = number % 10
    let remaining = number / 10

    match (number, digit, remaining) with
    | (0, 0, 0) -> [ 0 ]
    | (_, 0, 0) -> []
    | (_, _, 0) -> [ digit ]
    | _ -> List.append [ digit ] (digitize remaining)

let calcArmstrong (digits: int list): int =
    digits |> List.sumBy (fun digit -> pown digit digits.Length)

let isArmstrongNumber (number: int): bool = 
    (digitize number |> calcArmstrong) = number
