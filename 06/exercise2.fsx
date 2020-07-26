open System.IO

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

let orbits = loadOrbits "simple-input2.txt"
let graph = Graph.build orbits
