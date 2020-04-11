converttointlist(line) = map(x -> parse(Int, x), split(line))


function readintervals(nrsegments)
    segments = Array{Int}(undef, (nrsegments, 2))
    for ix in 1:nrsegments
        segments[ix, :] = readline() |> converttointlist
    end
    segments
end


function nrsuitableendpoints(endpoints, size, point)
    function search(a, b)
        if a == b
            return a
        end
        mid = (a + b) ÷ 2
        endpoints[mid] <= point ? search(mid + 1, b) : search(a, mid)
    end

    endpoints[end] <= point ? size : search(1, size) - 1
end


function intersectionsize(sortedleft, sortednegatedright, nrsegments, point)
    goodleftends = nrsuitableendpoints(sortedleft, nrsegments, point)
    goodrightends = nrsuitableendpoints(sortednegatedright, nrsegments, -point)
    goodleftends + goodrightends - nrsegments
end


function nrsegmentscontainingpoints(segments, nrsegments, points)
    sortedleft = sort(segments[:, 1])
    sortednegatedright = sort(-segments[:, 2])
    map(p -> intersectionsize(sortedleft, sortednegatedright, nrsegments, p), points)
end


function main()
    nrsegments, _ = readline() |> converttointlist
    segments = readintervals(nrsegments)
    points = readline() |> converttointlist
    result = nrsegmentscontainingpoints(segments, nrsegments, points)
    println(join(result, ' '))
end


main()
