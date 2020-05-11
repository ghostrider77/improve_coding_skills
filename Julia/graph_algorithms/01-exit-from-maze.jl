struct Graph
    adjacencylist::Vector{Set{Int}}

    function Graph(n, edges)
        adjacencylist = map(_ -> Set{Int}(), 1:n)
        for (n₁, n₂) in edges
            push!(adjacencylist[n₁], n₂)
            push!(adjacencylist[n₂], n₁)
        end
        new(adjacencylist)
    end
end


converttointlist(line) = map(x -> parse(Int, x), split(line))


function readedges(nredges)
    edges = Tuple{Int, Int}[]
    for _ in 1:nredges
        a, b = readline() |> converttointlist
        push!(edges, (a, b))
    end
    edges
end


function unvisitedneighbours(current, graph, visited)
    unvisited = Set{Int}()
    for node in current
        neighbours = graph.adjacencylist[node]
        for neighbour in neighbours
            if neighbour ∉ visited
                push!(unvisited, neighbour)
            end
        end
    end
    unvisited
end


function isconnectedbypath(n, edges, vₛ, vₑ)
    graph = Graph(n, edges)
    current = Set(vₛ)
    visited = Set(vₛ)
    while !isempty(current)
        unvisited = unvisitedneighbours(current, graph, visited)
        if vₑ ∈ unvisited
            return true
        end
        current = unvisited
        union!(visited, unvisited)
    end
    false
end


function main()
    nrnodes, nredges = readline() |> converttointlist
    edgelist = readedges(nredges)
    vₛ, vₑ = readline() |> converttointlist
    result = isconnectedbypath(nrnodes, edgelist, vₛ, vₑ)
    println(Int(result))
end


main()
