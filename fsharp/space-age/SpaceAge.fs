module SpaceAge

type Planet = 
    | Earth
    | Jupiter
    | Mars
    | Mercury
    | Neptune
    | Saturn
    | Uranus
    | Venus

let age (planet: Planet) (seconds: int64): float = 
    let earthAge = (float) seconds / 31557600.0

    let orbitalPeriod =
        match planet with
        | Earth -> 1.0
        | Jupiter -> 11.862615
        | Mars -> 1.8808158
        | Mercury -> 0.2408467
        | Neptune -> 164.79132
        | Saturn -> 29.447498
        | Uranus -> 84.016846 
        | Venus -> 0.61519726

    earthAge / orbitalPeriod
