converttointlist(line) = map(x -> parse(Int, x), split(line))


function pisanoperiod(modulus)
    p = 1
    a, b = 1, 1
    while !(a == 0 && b == 1)
        a, b = b, (a + b) % modulus
        p += 1
    end
    p
end


function fibonaccimodulo(n, modulus)
    k = 1
    a, b = 0, 1
    while k <= n
        a, b = b, (a + b) % modulus
        k += 1
    end
    a
end


function hugefibonaccimodulo(n, modulus)
    p = pisanoperiod(modulus)
    fibonaccimodulo(n % p, modulus)
end


function main()
    n, modulus = readline() |> converttointlist
    result = hugefibonaccimodulo(n, modulus)
    println(result)
end


main()
