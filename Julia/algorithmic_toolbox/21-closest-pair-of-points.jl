const bruteforcesize = 3

converttointlist(line) = map(x -> parse(Int, x), split(line))


distance(p, q) = hypot(p.x - q.x, p.y - q.y)


function readpoints(n)
    points = NamedTuple{(:x, :y), Tuple{Int, Int}}[]
    for _ in 1:n
        a, b = readline() |> converttointlist
        push!(points, (x=a, y=b))
    end
    points
end


function smallestpairwisedistance(points; mindist, comparewith)
    for (ix, p) in enumerate(points)
        nrnextpoints = min(ix + 1 + comparewith, length(points))
        for jy in (ix+1:nrnextpoints)
            q = points[jy]
            d = distance(p, q)
            if d < mindist
                mindist = d
            end
        end
    end
    mindist
end


function pointsinstripe(xs₁, xs₂, median, δ)
    stripe = NamedTuple{(:x, :y), Tuple{Int, Int}}[]
    function addpoints(halfplane)
        for p in halfplane
            if abs(p.x - median) <= δ
                push!(stripe, p)
            end
        end
    end

    addpoints(xs₁)
    addpoints(xs₂)
    stripe
end


function mimimumdistanceinstripe(xs₁, xs₂, median, δ)
    stripe = sort(pointsinstripe(xs₁, xs₂, median, δ); by=p -> p.y)
    smallestpairwisedistance(stripe; mindist=δ, comparewith=7)
end


function closestpoints(sortedpoints, n)
    if n <= bruteforcesize
        return smallestpairwisedistance(sortedpoints; mindist=Inf, comparewith=bruteforcesize-1)
    end
    middle = (n + 1) ÷ 2
    medianₓ = sortedpoints[middle].x
    xs₁, xs₂ = sortedpoints[1:middle], sortedpoints[middle+1:end]
    δ₁ = closestpoints(xs₁, middle)
    δ₂ = closestpoints(xs₂, n - middle)
    δ = min(δ₁, δ₂)
    abs(δ) < 1e-14 ? 0.0 : mimimumdistanceinstripe(xs₁, xs₂, medianₓ, δ)
end


function closestpairofpoints(points, n)
    sortedpoints = sort(points; by=p -> p.x)
    closestpoints(sortedpoints, n)
end


function main()
    n = parse(Int, readline())
    points = readpoints(n)
    dist = closestpairofpoints(points, n)
    println(dist)
end


main()
