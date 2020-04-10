function decomposition_maximalnumber_distinctelems(n)
    summands = []
    smallestsummand = 1
    while n > 0
        elem = n > 2*smallestsummand ? smallestsummand : n
        push!(summands, elem)
        n -= elem
        smallestsummand += 1
    end
    summands
end


function main()
    n = parse(Int, readline())
    result = decomposition_maximalnumber_distinctelems(n)
    println(length(result))
    println(join(result, ' '))
end


main()
