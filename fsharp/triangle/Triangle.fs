module Triangle

let equilateral (triangle: float list) : bool = 
    match triangle with
    | [0.; 0.; 0.] -> false
    | [a; b; c] when a = b && b = c -> true
    | _ -> false
 
let isosceles triangle = failwith "You need to implement this function."

let scalene triangle = failwith "You need to implement this function."