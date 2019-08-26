module Triangle

let isLengthOfTwoSidesGreaterOrEqual (triangle: float list) (side: int): bool =
    let permuted = triangle |> List.permute (fun i -> (i + side) % 3)
    let firstSideLength = permuted.Head
    let otherSidesLength = permuted.Tail |> List.sum
    otherSidesLength >= firstSideLength

let isTriangleValid (triangle: float list) : bool =
    [ for side in 0 .. 2 -> isLengthOfTwoSidesGreaterOrEqual triangle side ] 
    |> List.exists (fun x -> x = false)
    |> not

let (|Valid|Invalid|) (triangle: float list) =
    match triangle with
    | t when isTriangleValid t -> Valid
    | _ -> Invalid

let (|Equilateral|Isosceles|Scalene|) (triangle: float list) =
    let distinctLengths = triangle |> List.distinct
    match distinctLengths.Length with
    | d when d = 1 && distinctLengths.Head <> 0. -> Equilateral
    | 2 -> Isosceles
    | _ -> Scalene

let equilateral (triangle: float list) : bool = 
    match triangle with
    | Valid & Equilateral -> true
    | _ -> false
 
let isosceles triangle = 
    match triangle with
    | Valid & (Isosceles | Equilateral) -> true
    | _ -> false

let scalene triangle = 
    match triangle with
    | Valid & Scalene -> true
    | _ -> false
