converttointlist(line) = map(x -> parse(Int, x), split(line))


function minindex(array, parentindex, size)
    minindex = parentindex
    leftchild = 2 * parentindex
    if leftchild <= size && array[leftchild] < array[minindex]
        minindex = leftchild
    end
    rightchild = leftchild + 1
    if rightchild <= size && array[rightchild] < array[minindex]
        minindex = rightchild
    end
    minindex
end


function siftdown(array, parentindex, size, swaps)
    index = minindex(array, parentindex, size)
    while index != parentindex
        array[index], array[parentindex] = array[parentindex], array[index]
        push!(swaps, (parentindex-1, index-1))
        parentindex = index
        index = minindex(array, parentindex, size)
    end
end


function heapify(array, size)
    swaps = Tuple{Int, Int}[]
    for parentindex = (size ÷ 2):-1:1
        siftdown(array, parentindex, size, swaps)
    end
    swaps
end


function main()
    n = parse(Int, readline())
    array = readline() |> converttointlist
    swaps = heapify(array, n)
    println(length(swaps))
    for swap in swaps
        println(join(swap, " "))
    end
end


main()
