struct DirectedGraph
    nrnodes::Int
    adjacencylist::Vector{Set{Int}}

    function DirectedGraph(n::Int, edges::Vector{Tuple{Int, Int}})
        adjacencylist = map(_ -> Set{Int}(), 1:n)
        for (n₁, n₂) in edges
            push!(adjacencylist[n₁], n₂)
        end
        new(n, adjacencylist)
    end
end


mutable struct DFSState
    previsit_numbers::Vector{Int}
    postvisit_numbers::Vector{Int}
    previsit_id::Int
    postvisit_id::Int

    DFSState(n::Int) = new(zeros(Int, n), zeros(Int, n), 1, 1)
end


function depthfirstsearch(graph::DirectedGraph)::DFSState
    state = DFSState(graph.nrnodes)
    for startnode in 1:graph.nrnodes
        if !isvisited(startnode, state)
            _ = explore(startnode, state, graph)
        end
    end
    state
end


isvisited(node::Int, state::DFSState)::Bool = state.previsit_numbers[node] != 0


function explore(node::Int, state::DFSState, graph::DirectedGraph)::Set{Int}
    state.previsit_numbers[node] = state.previsit_id
    state.previsit_id += 1
    stack = Vector{Int}([node])
    component = Set{Int}(node)
    while !isempty(stack)
        lastnode = pop!(stack)
        unvisited = findunvisitedneighbour(lastnode, graph, state)
        if unvisited === nothing
            state.postvisit_numbers[lastnode] = state.postvisit_id
            state.postvisit_id += 1
        else
            state.previsit_numbers[unvisited] = state.previsit_id
            state.previsit_id += 1
            push!(stack, lastnode)
            push!(stack, unvisited)
            push!(component, unvisited)
        end
    end
    component
end


function findunvisitedneighbour(node::Int, graph::DirectedGraph, state::DFSState)::Union{Nothing, Int}
    neighbours = graph.adjacencylist[node]
    for neighbour in neighbours
        if !isvisited(neighbour, state)
            return neighbour
        end
    end
    return nothing
end


converttointlist(line) = map(x -> parse(Int, x), split(line))


function readedges(nredges)
    edges = Tuple{Int, Int}[]
    for _ in 1:nredges
        from, to = readline() |> converttointlist
        push!(edges, (from, to))
    end
    edges
end


function hascycle(graph)
    finalstate = depthfirstsearch(graph)
    for (node, neighbours) in enumerate(graph.adjacencylist)
        nodenumber = finalstate.postvisit_numbers[node]
        if any([finalstate.postvisit_numbers[n] >= nodenumber for n in neighbours])
            return true
        end
    end
    false
end


function main()
    nrnodes, nredges = readline() |> converttointlist
    edges = readedges(nredges)
    graph = DirectedGraph(nrnodes, edges)
    result = hascycle(graph)
    println(Int(result))
end


main()
