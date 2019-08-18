module Darts
open System

let score (x: double) (y: double): int =
    let radius = Math.Sqrt (x * x + y * y)
    
    match radius with
        | r when r > 10.0 -> 0
        | r when r > 5.0 -> 1
        | r when r > 1.0 -> 5
        | _ -> 10
        