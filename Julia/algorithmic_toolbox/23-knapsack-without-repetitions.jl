converttointlist(line) = map(x -> parse(Int, x), split(line))


function solveknapsack(weights, nrweights, capacity)
    knapsack = (-1)*ones(Int, (capacity, nrweights))
    function solve(currentcapacity, n)
        if currentcapacity == 0 || n == 0
            return 0
        end
        if knapsack[currentcapacity, n] != -1
            return knapsack[currentcapacity, n]
        end

        weight = weights[n]
        optimalweight = begin
            if currentcapacity < weight
                solve(currentcapacity, n - 1)
            else
                max(solve(currentcapacity - weight, n - 1) + weight, solve(currentcapacity, n - 1))
            end
        end
        knapsack[currentcapacity, n] = optimalweight
        optimalweight
    end

    solve(capacity, nrweights)
end


function main()
    capacity, nrweights = readline() |> converttointlist
    weights = readline() |> converttointlist
    result = solveknapsack(weights, nrweights, capacity)
    println(result)
end


main()
