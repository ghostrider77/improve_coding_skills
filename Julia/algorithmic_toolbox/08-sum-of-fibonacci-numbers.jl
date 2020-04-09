const modulus = 10


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


function lastdigitofthesumoffibonacci(n)
    p = pisanoperiod(modulus)
    mod(fibonaccimodulo((n + 2) % p, modulus) - 1, modulus)
end


function main()
    n = parse(Int, readline())
    digit = lastdigitofthesumoffibonacci(n)
    println(digit)
end


main()
