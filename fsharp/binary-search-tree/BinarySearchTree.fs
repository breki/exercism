module BinarySearchTree

type Node = {
        Value: int
        Left: Node option
        Right: Node option
    }

let left (node: Node) : Node option = 
    node.Left

let right (node: Node) : Node option = 
    node.Right

let data (node: Node) : int =
    node.Value

let rec build (items: int list) : Node option =
    match items.Length with
    | 0 -> None
    | _ ->
        let first = items |> List.head

        Some { 
            Value = items |> List.head; 
            Left = 
                items 
                |> List.skip 1 
                |> List.filter (fun x -> x <= first) 
                |> build;
            Right =
                items 
                |> List.skip 1 
                |> List.filter (fun x -> x > first) 
                |> build
        }


let create (items: int list) : Node =
    let first = items |> List.head

    { 
        Value = items |> List.head; 
        Left = 
            items 
            |> List.skip 1 
            |> List.filter (fun x -> x <= first) 
            |> build;
        Right =
            items 
            |> List.skip 1 
            |> List.filter (fun x -> x > first) 
            |> build
    }

let rec sortedData (node: Node): int list = 
    let listLeft = 
        match node.Left with
        | None -> [ node.Value ]
        | Some leftNode -> List.append (sortedData leftNode) [ node.Value ]

    match node.Right with
    | None -> listLeft
    | Some rightNode -> List.append listLeft (sortedData rightNode)

