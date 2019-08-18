module RobotName

type Robot (name: string) =
    member this.Name = name

let mutable usedRobotNames: Set<string> = Set.empty

let rec randomRobotName(): string =
    let rnd = System.Random()
    let alpha = [| 'A' .. 'Z' |]
    let num = [| '0' .. '9' |]
    let name =
        [|
          alpha.[rnd.Next alpha.Length];
          alpha.[rnd.Next alpha.Length];
          num.[rnd.Next num.Length];
          num.[rnd.Next num.Length];
          num.[rnd.Next num.Length]
        |]
    let candidateName = new string(name)
    if Set.contains candidateName usedRobotNames then
        randomRobotName()
    else
        let chosenName = candidateName
        usedRobotNames <- Set.add chosenName usedRobotNames
        chosenName

let mkRobot() =
    Robot(randomRobotName())

let name (robot: Robot) =
    robot.Name

let reset robot =
    mkRobot()