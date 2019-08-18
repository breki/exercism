module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int =
    let numbersCleaned = numbers |> List.filter (fun x -> x > 0)

    match numbersCleaned with
    | [] -> 0
    | _ ->
        let lowestNumber = 
            List.sort numbersCleaned
            |> List.head

        let maxMultiplicatorNeeded = 
            ((upperBound - 1) / lowestNumber + 1)

        // multiplication integers
        let multiplicators = [| 1 .. maxMultiplicatorNeeded |]
    
        // we only use numbers that are below the upper bound
        let numbersWithinBounds = 
            numbersCleaned |> List.filter (fun x -> x < upperBound)

        // set of all unique multiples
        let uniqueMultiples = 
            [ for number in numbersWithinBounds do
                for multiplicator in multiplicators do
                    let value = number * multiplicator
                    if value < upperBound then
                        yield number * multiplicator ]
            |> Set.ofList

        // sum them up
        uniqueMultiples |> Set.toSeq |> Seq.sum
