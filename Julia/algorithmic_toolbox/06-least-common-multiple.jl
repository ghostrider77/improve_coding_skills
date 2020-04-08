converttointlist(line) = map(x -> parse(Int, x), split(line))


function calcgcd(a, b)
    while b > 0
        a, b = b, a % b
    end
    a
end


function calclcm(a, b)
    gcd = calcgcd(a, b)
    (a ÷ gcd) * b
end


function main()
    a, b = readline() |> converttointlist
    result = calclcm(a, b)
    println(result)
end


main()
