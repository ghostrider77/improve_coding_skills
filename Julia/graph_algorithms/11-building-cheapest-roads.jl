struct Point
    x::Int
    y::Int
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


function minimalspanningtree(points)
    nodeswithcost = Dict(point => Inf for point in points)
    startnode = points[1]
    nodeswithcost[startnode] = 0
    totalcost = 0
    while !isempty(nodeswithcost)
        v = argmin(nodeswithcost)
        costofadding_v = pop!(nodeswithcost, v)
        totalcost += costofadding_v
        for (z, costofadding_z) in nodeswithcost
            dist = distance(v, z)
            if costofadding_z > dist
                nodeswithcost[z] = dist
            end
        end
    end
    totalcost
end


function main()
    n = parse(Int, readline())
    points = readpoints(n)
    result = minimalspanningtree(points)
    println(result)
end


main()
