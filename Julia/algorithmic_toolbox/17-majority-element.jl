converttointlist(line) = map(x -> parse(Int, x), split(line))


function hasmajorityelem(list, n)
    counter = begin
        dict = Dict{Int, Int}()
        for x in list
            if haskey(dict, x)
                dict[x] += 1
            else
                dict[x] = 1
            end
        end
        dict
    end

    half = n ÷ 2
    for count in values(counter)
        if count > half
            return 1
        end
    end
    0
end


function main()
    n = parse(Int, readline())
    list = readline() |> converttointlist
    result = hasmajorityelem(list, n)
    println(result)
end


main()
