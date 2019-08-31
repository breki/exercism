module GradeSchool

type School = Map<int, string list>

let empty: School = 
    Map.empty

let add (student: string) (grade: int) (school: School): School = 
    match school.TryGetValue grade with
    | true, existingStudents -> school.Add (grade, student :: existingStudents)
    | _ -> school.Add (grade, [ student ])

let roster (school: School): string list = 
    school 
    |> Map.toList
    |> List.sortBy (fun (grade, _) -> grade)
    |> List.map (fun (_, students) -> students |> List.sort)
    |> List.concat

let grade (number: int) (school: School): string list = 
    match school.TryGetValue number with
    | true, students -> students |> List.sort
    | _ -> []
