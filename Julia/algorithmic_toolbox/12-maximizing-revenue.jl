converttointlist(line) = map(x -> parse(Int, x), split(line))


function maximalrevenue(profitperclick, averageclick)
    revenue = 0
    for (p, a) in zip(sort(profitperclick), sort(averageclick))
        revenue += p*a
    end
    revenue
end


function main()
    lines = readlines()
    profitperclick = lines[2] |> converttointlist
    averageclick = lines[3] |> converttointlist
    result = maximalrevenue(profitperclick, averageclick)
    println(result)
end


main()
