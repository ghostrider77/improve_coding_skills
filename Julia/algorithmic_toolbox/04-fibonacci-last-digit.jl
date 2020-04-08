const modulus = 10


function fibonaccilastdigit(n)
    k = 1
    a, b = 0, 1
    while k <= n
        a, b = b, (a + b) % modulus
        k += 1
    end
    a
end


function main()
    n = parse(Int, readline())
    digit = fibonaccilastdigit(n)
    println(digit)
end


main()
