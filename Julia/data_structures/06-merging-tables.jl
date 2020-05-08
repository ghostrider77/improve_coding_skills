struct UnionFind
    tablerows::Vector{Int}
    parents::Vector{Int}
    ranks::Vector{Int}

    function UnionFind(tablerows::Vector{Int}, nrtables::Int)
        parents = collect(1:nrtables)
        ranks = zeros(Int, nrtables)
        new(tablerows, parents, ranks)
    end
end


function find(tables::UnionFind, childindex::Int)::Int
    parentindex = tables.parents[childindex]
    nodesonpath = Int[]
    while childindex != parentindex
        push!(nodesonpath, childindex)
        childindex = parentindex
        parentindex = tables.parents[childindex]
    end
    root = parentindex
    for ix in nodesonpath
        tables.parents[ix] = root
    end
    root
end


function union(tables::UnionFind, src::Int, dest::Int, largest_tablesize::Int)::Int
    ids = find(tables, src)
    idd = find(tables, dest)
    if ids == idd
        return largest_tablesize
    end
    if tables.ranks[ids] > tables.ranks[idd]
        tables.parents[idd] = ids
        tables.tablerows[ids] += tables.tablerows[idd]
        tables.tablerows[idd] = 0
        max(largest_tablesize, tables.tablerows[ids])
    else
        tables.parents[ids] = idd
        tables.tablerows[idd] += tables.tablerows[ids]
        tables.tablerows[ids] = 0
        if tables.ranks[ids] == tables.ranks[idd]
            tables.ranks[idd] += 1
        end
        max(largest_tablesize, tables.tablerows[idd])
    end
end


converttointlist(line) = map(x -> parse(Int, x), split(line))


function readtableops(nrops)
    operations = NamedTuple{(:dest, :src), Tuple{Int, Int}}[]
    for _ in 1:nrops
        d, s = readline() |> converttointlist
        push!(operations, (dest=d, src=s))
    end
    operations
end


function processmergerequests(tablerows, nrtables, operations)
    tables = UnionFind(tablerows, nrtables)
    largest_tablesize = maximum(tablerows)
    maximal_tablesizes = Int[]
    for (dest, src) in operations
        largest_tablesize = union(tables, src, dest, largest_tablesize)
        push!(maximal_tablesizes, largest_tablesize)
    end
    maximal_tablesizes
end


function main()
    nrtables, nroperations = readline() |> converttointlist
    tablerows = readline() |> converttointlist
    operations = readtableops(nroperations)
    result = processmergerequests(tablerows, nrtables, operations)
    println(join(result, "\n"))
end


main()
