﻿module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int =
    let numbersCleaned = numbers |> List.filter (fun x -> x > 0)

    let numbersWithinBounds = 
        numbersCleaned |> List.filter (fun x -> x < upperBound)

    [ for number in numbersWithinBounds do
        let maxMultiplicator = (upperBound - 1) / number;

        for multiplicator in [| 1 .. maxMultiplicator |] do
            let value = number * multiplicator
            yield value 
        ]
    |> Set.ofList
    |> Set.toSeq 
    |> Seq.sum
