const nominators = (2, 3)


function findpreviousminimum(operations, k)
    m = k - 1
    previousminimum = operations[m]
    minarg = m
    for nom in nominators
        if k % nom == 0
            m = k ÷ nom
            nrops = operations[m]
            if nrops < previousminimum
                previousminimum = nrops
                minarg = m
            end
        end
    end
    previousminimum, minarg
end


function backtrackcalculations(backtrack, n)
    path = [n]
    k = n
    while k > 1
        k = backtrack[k]
        push!(path, k)
    end
    reverse(path)
end


function runcalculator(n)
    minoperations = zeros(Int, n)
    backtrack = zeros(Int, n)
    for k in 2:n
        previousmin, ix = findpreviousminimum(minoperations, k)
        minoperations[k] = previousmin + 1
        backtrack[k] = ix
    end
    backtrackcalculations(backtrack, n)
end


function main()
    n = parse(Int, readline())
    result = runcalculator(n)
    println(length(result) - 1)
    println(join(result, ' '))
end


main()

