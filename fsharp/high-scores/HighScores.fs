module HighScores

let scores (values: int list): int list = 
    values

let latest (values: int list): int = 
    values |> List.last

let personalBest (values: int list): int = 
    values |> List.max

let personalTopThree (values: int list): int list = 
    let itemsToTake = 
        min 3 (List.length values)

    values 
    |> List.sortDescending 
    |> List.take itemsToTake