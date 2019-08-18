module TwelveDays

let recite start stop = 
    let days = 
        [| 
        "first"; "second"; "third"; "fourth"; "fifth";
        "sixth"; "seventh"; "eighth"; "ninth"; "tenth"; "eleventh";
        "twelfth"
        |]

    let objects =
        [|
        "a Partridge in a Pear Tree"; "two Turtle Doves"; "three French Hens";
        "four Calling Birds"; "five Gold Rings"; "six Geese-a-Laying";
        "seven Swans-a-Swimming"; "eight Maids-a-Milking"; "nine Ladies Dancing";
        "ten Lords-a-Leaping"; "eleven Pipers Piping"; "twelve Drummers Drumming"
        |]

    let listObjects(day: int): string =
        match day with
        | 1 -> objects.[0]
        | _ ->
            let firstPart = 
                Array.sub objects 1 (day-1) 
                |> Array.rev
                |> String.concat ", "
            firstPart + ", and " + objects.[0]

    let construct day: string =
        let dayText: string = days.[day - 1]
        let objectsText = listObjects day
        sprintf 
            "On the %s day of Christmas my true love gave to me: %s." 
            dayText 
            objectsText

    [ for day in start .. stop -> construct day ]
