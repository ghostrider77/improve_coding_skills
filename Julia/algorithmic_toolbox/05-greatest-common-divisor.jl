converttointlist(line) = map(x -> parse(Int, x), split(line))


function calcgcd(a, b)
    while b > 0
        a, b = b, a % b
    end
    a
end


function main()
    a, b = readline() |> converttointlist
    result = calcgcd(a, b)
    println(result)
end


main()
