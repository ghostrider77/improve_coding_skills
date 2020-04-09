converttointlist(line) = map(x -> parse(Int, x), split(line))


function readitems(nritems)
    items = []
    for _ in 1:nritems
        value, weight = readline() |> converttointlist
        push!(items, (value=value, weight=weight))
    end
    items
end


function fractionalknapsack(items, capacity)
    sorteditems = sort(items; by=x -> x.value / x.weight, rev=true)
    totalvalue = 0
    for (value, weight) in sorteditems
        if capacity == 0
            return totalvalue
        end
        amount = min(weight, capacity)
        totalvalue += amount * (value / weight)
        capacity -= amount
    end
    totalvalue
end


function main()
    nritems, capacity = readline() |> converttointlist
    items = readitems(nritems)
    value = fractionalknapsack(items, capacity)
    println(value)
end


main()
