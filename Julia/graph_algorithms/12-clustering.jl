IntOption = Union{Int, Nothing}

struct Point
    x::Int
    y::Int
end


struct UnionFind
    nrpoints::Int
    parents::Vector{Int}
    ranks::Vector{Int}

    UnionFind(nrpoints::Int) = new(nrpoints, collect(1:nrpoints), zeros(Int, nrpoints))
end


function find(unionfind::UnionFind, childindex::Int)::Int
    parentindex = unionfind.parents[childindex]
    nodesonpath = Int[]
    while childindex != parentindex
        push!(nodesonpath, childindex)
        childindex = parentindex
        parentindex = unionfind.parents[childindex]
    end
    root = parentindex
    for ix in nodesonpath
        unionfind.parents[ix] = root
    end
    root
end


function union(unionfind::UnionFind, p_ix::Int, q_ix::Int, parent_p::IntOption, parent_q::IntOption)::Nothing
    if parent_p === nothing
        parent_p = find(unionfind, p_ix)
    end
    if parent_q === nothing
        parent_q = find(unionfind, q_ix)
    end

    if parent_p == parent_q
        return nothing
    end

    if unionfind.ranks[parent_p] > unionfind.ranks[parent_q]
        unionfind.parents[parent_q] = parent_p
    else
        unionfind.parents[parent_p] = parent_q
        if unionfind.ranks[parent_p] == unionfind.ranks[parent_q]
            unionfind.ranks[parent_q] += 1
        end
    end
    return nothing
end


distance(p::Point, q::Point) = hypot(p.x - q.x, p.y - q.y)


converttointlist(line) = map(x -> parse(Int, x), split(line))


function readpoints(n)
    points = Point[]
    for _ in 1:n
        x, y = readline() |> converttointlist
        push!(points, Point(x, y))
    end
    points
end


function sortedpairwisedistances(points)
    distances = NamedTuple{(:p_ix, :q_ix, :dist), Tuple{Int, Int, Float64}}[]
    for (ix, p) in enumerate(points), (jy, q) in enumerate(points[ix+1:end])
        push!(distances, (p_ix=ix, q_ix=ix+jy, dist=distance(p, q)))
    end
    sort(distances; by=x -> x.dist)
end


function optimalclustering(nrpoints, points, k)
    distances = sortedpairwisedistances(points)
    clusters = UnionFind(nrpoints)
    nrclusters = nrpoints
    for (p_ix, q_ix, dist) in distances
        cluster_p = find(clusters, p_ix)
        cluster_q = find(clusters, q_ix)
        if cluster_p != cluster_q
            union(clusters, p_ix, q_ix, cluster_p, cluster_q)
            nrclusters -= 1
            if nrclusters == k - 1
                return dist
            end
        end
    end
end


function main()
    nrpoints = parse(Int, readline())
    points = readpoints(nrpoints)
    k = parse(Int, readline())
    d = optimalclustering(nrpoints, points, k)
    println(d)
end


main()
