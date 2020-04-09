const modulus = 10

converttointlist(line) = map(x -> parse(Int, x), split(line))


function pisanoperiod(m)
    p = 1
    a, b = 1, 1
    while !(a == 0 && b == 1)
        a, b = b, (a + b) % m
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


function lastdigitofpartialsum(m, n)
    p = pisanoperiod(modulus)
    lastdigitₘ₋₁ = mod(fibonaccimodulo((m + 1) % p, modulus) - 1, modulus)
    lastdigitₙ = mod(fibonaccimodulo((n + 2) % p, modulus) - 1, modulus)
    mod(lastdigitₙ - lastdigitₘ₋₁, modulus)
end


function main()
    m, n = readline() |> converttointlist
    digit = lastdigitofpartialsum(m, n)
    println(digit)
end


main()
