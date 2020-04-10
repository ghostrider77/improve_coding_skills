using Random


converttointlist(line) = map(x -> parse(Int, x), split(line))


function quicksort!(list, n)
    function threewaypartition(pivot, start, last)
        ix = start
        while ix <= last
            elem = list[ix]
            if elem < pivot
                if ix != start
                    list[ix], list[start] = list[start], list[ix]
                end
                ix += 1
                start += 1
            elseif elem > pivot
                list[ix], list[last] = list[last], elem
                last -= 1
            else
                ix += 1
            end
        end
        start, last
    end

    stack = [(1, n)]
    while !isempty(stack)
        leftend, rightend = pop!(stack)
        if leftend < rightend
            randomix = Random.rand(leftend:rightend)
            pivot = list[randomix]
            midstart, midend = threewaypartition(pivot, leftend, rightend)
            push!(stack, (leftend, midstart-1))
            push!(stack, (midend+1, rightend))
        end
    end
end


function main()
    n = parse(Int, readline())
    list = readline() |> converttointlist
    Random.seed!(2112)
    quicksort!(list, n)
    println(join(list, " "))
end


main()
