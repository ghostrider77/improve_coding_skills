converttointlist(line) = map(x -> parse(Int, x), split(line))


function mergesortedarrays(sorted₁, sorted₂, n₁, n₂, inversions)
    merged = Vector{Int}(undef, n₁+n₂)
    ix = 1
    jy = 1
    for k in 1:(n₁+n₂)
        if ix > n₁ && jy <= n₂
            merged[k] = sorted₂[jy]
            jy += 1
        elseif ix <= n₁ && jy > n₂
            merged[k] = sorted₁[ix]
            ix += 1
        elseif sorted₁[ix] <= sorted₂[jy]
            merged[k] = sorted₁[ix]
            ix += 1
        else
            merged[k] = sorted₂[jy]
            jy += 1
            inversions += (n₁ - ix + 1)
        end
    end
    merged, inversions
end


function countinversions(list, n)
    if n <= 1
        return list, 0
    end
    middle = n ÷ 2
    xs₁, xs₂ = list[1:middle], list[middle+1:end]
    n₁, n₂ = middle, n - middle
    sorted₁, inversions₁ = countinversions(xs₁, n₁)
    sorted₂, inversions₂ = countinversions(xs₂, n₂)
    mergesortedarrays(sorted₁, sorted₂, n₁, n₂, inversions₁+inversions₂)
end


function main()
    n = parse(Int, readline())
    list = readline() |> converttointlist
    _, result = countinversions(list, n)
    println(result)
end


main()
