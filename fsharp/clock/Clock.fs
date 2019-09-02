module Clock

type Clock = { Hours : int; Minutes: int }

let create hours minutes = 
    let hoursOfMinutes = minutes / 60
    let minutesRemainder = minutes % 60
    let (minutesFinal, hourSubtracted) =
        match minutesRemainder with
        | x when x < 0 -> (x + 60, 1)
        | x -> (x, 0)

    let hoursTotal = (hours + hoursOfMinutes - hourSubtracted) % 24
    let hoursFinal = 
        match hoursTotal with
        | x when x < 0 -> x + 24
        | x -> x

    { Hours = hoursFinal; Minutes = minutesFinal }

let add minutes clock = 
    create clock.Hours (clock.Minutes + minutes)

let subtract minutes clock = 
    add -minutes clock

let display clock = 
    sprintf "%02d:%02d" clock.Hours clock.Minutes