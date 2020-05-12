struct Graph
    nrnodes::Int
    adjacencylist::Vector{Set{Int}}

    function Graph(n::Int, edges::Vector{Tuple{Int, Int}})
        adjacencylist = map(_ -> Set{Int}(), 1:n)
        for (n₁, n₂) in edges
            push!(adjacencylist[n₁], n₂)
            push!(adjacencylist[n₂], n₁)
        end
        new(n, adjacencylist)
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


function connected_components(graph)
    previsit_numbers = zeros(Int, graph.nrnodes)
    postvisit_numbers = zeros(Int, graph.nrnodes)
    previsit_id = 1
    postvisit_id = 1
    components = Set{Int}[]
    isvisited(node::Int) = previsit_numbers[node] != 0

    function findunvisitedneighbour(node::Int)
        neighbours = graph.adjacencylist[node]
        for neighbour in neighbours
            if !isvisited(neighbour)
                return neighbour
            end
        end
        return nothing
    end

    function explore(node::Int)::Set{Int}
        previsit_numbers[node] = previsit_id
        previsit_id += 1
        stack = Vector{Int}([node])
        component = Set{Int}(node)
        while !isempty(stack)
            lastnode = pop!(stack)
            unvisited = findunvisitedneighbour(lastnode)
            if unvisited == nothing
                postvisit_numbers[lastnode] = postvisit_id
                postvisit_id += 1
            else
                previsit_numbers[unvisited] = previsit_id
                previsit_id += 1
                push!(stack, lastnode)
                push!(stack, unvisited)
                push!(component, unvisited)
            end
        end
        component
    end

    for startnode in 1:graph.nrnodes
        if !isvisited(startnode)
            component = explore(startnode)
            push!(components, component)
        end
    end
    components
end


function main()
    nrnodes, nredges = readline() |> converttointlist
    edges = readedges(nredges)
    graph = Graph(nrnodes, edges)
    components = connected_components(graph)
    println(length(components))
end


main()
