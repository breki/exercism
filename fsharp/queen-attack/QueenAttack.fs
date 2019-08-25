module QueenAttack

let create (position: int * int) = 
    match position with
    | (x, _) when x < 0 || x >= 8 -> false
    | (_, y) when y < 0 || y >= 8 -> false
    | _ -> true

let canAttack (queen1: int * int) (queen2: int * int) = 
    let onDiagonal (x1, y1) (x2, y2) = 
        abs(x1 - x2) = abs(y1 - y2)

    match (queen1, queen2) with
    | ((x1, _), (x2, _)) when x1 = x2 -> true
    | ((_, y1), (_, y2)) when y1 = y2 -> true
    | _ -> onDiagonal queen1 queen2
