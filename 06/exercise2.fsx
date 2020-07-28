open System.IO
open System.Collections.Generic

let toTuple (arr: 't[]) =
    match arr with
    | [| a; b |] -> Some (a, b)
    | _ -> None

let loadOrbits filename =
    File.ReadLines filename
    |> Seq.map (fun s -> s.Split ')' |> toTuple)
    |> Seq.choose id
    |> Seq.toList

module Graph =
    type Graph = Map<string, Set<string>>

    let edges vertex (graph: Graph) =
        match graph.TryFind vertex with
        | Some edges -> edges
        | _ -> Set.empty

    let addVertex vertex (graph: Graph) =
        if not (graph.ContainsKey vertex) then
            graph.Add (vertex, Set.empty)
        else
            graph

    let addEdge vertexA vertexB (graph: Graph) =
        let vertexAEdges = graph |> edges vertexA |> Set.add vertexB
        let vertexBEdges = graph |> edges vertexB |> Set.add vertexA
        graph
        |> Map.add vertexA vertexAEdges
        |> Map.add vertexB vertexBEdges 

    let build edges =
        List.fold (fun graph (vertexA, vertexB) -> addEdge vertexA vertexB graph) Map.empty edges

    let shortestPath startingVertex goalVertex (graph: Graph) =
        let mutable distances: Map<string, int> = Map.empty |> Map.add startingVertex 0
        let queue = new Queue<string>()
        queue.Enqueue startingVertex
        let mutable finalDistance: int option = None

        while queue.Count > 0 && (Option.isNone finalDistance) do
            let vertex = queue.Dequeue()
            let distance = distances.Item vertex
            if vertex = goalVertex then 
                finalDistance <- Some distance
            else
                let unvisitedNeighbors =
                    edges vertex graph
                    |> Set.filter (fun n -> not (distances.ContainsKey n))
                for neighbor in unvisitedNeighbors do
                    distances <- distances |> Map.add neighbor (distance + 1)
                    queue.Enqueue neighbor

        finalDistance


let orbits = loadOrbits "input.txt"
let graph = Graph.build orbits

let shortestPath = Graph.shortestPath "YOU" "SAN" graph
let answer =
    match shortestPath with
    // Answer is distance between the objects SAN and YOU are orbiting, not distance between SAN and YOU
    | Some path -> path - 2 
    | None -> -1