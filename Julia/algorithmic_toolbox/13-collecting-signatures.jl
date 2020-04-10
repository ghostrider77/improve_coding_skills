converttointlist(line) = map(x -> parse(Int, x), split(line))


function readsegments(nrsegments)
    segments = []
    for _ in 1:nrsegments
        a, b = readline() |> converttointlist
        push!(segments, (left=a, right=b))
    end
    segments
end


function minnumberofpoints_covering_segments(segments)
    sortedsegments = sort(segments; by=s -> s.right)
    points = []
    while !isempty(sortedsegments)
        _, b = sortedsegments[1]
        push!(points, b)
        sortedsegments = [segment for segment in sortedsegments[2:end] if segment.left > b]
    end
    points
end


function main()
    nrsegments = parse(Int, readline())
    segments = readsegments(nrsegments)
    covering = minnumberofpoints_covering_segments(segments)
    println(length(covering))
    println(join(covering, ' '))
end


main()
