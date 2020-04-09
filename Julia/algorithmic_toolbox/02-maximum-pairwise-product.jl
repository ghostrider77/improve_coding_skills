converttointlist(line) = map(x -> parse(Int, x), split(line))


function maxpairwiseproduct(list)
    function processnextelem(acc, elem)
        if elem > acc.largest
            (largest=elem, secondlargest=acc.largest)
        elseif elem > acc.secondlargest
            (largest=acc.largest, secondlargest=elem)
        else
            acc
        end
    end

    result = foldl(processnextelem, list; init=(largest=typemin(Int), secondlargest=typemin(Int)))
    prod(result)
end


function main()
    list = readline() |> converttointlist
    result = maxpairwiseproduct(list)
    print(result)
end


main()
