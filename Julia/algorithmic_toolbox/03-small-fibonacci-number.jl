function fibonacci(n)
    k = 1
    a, b = 0, 1
    while k <= n
        a, b = b, a + b
        k += 1
    end
    a
end


function main()
    n = parse(Int, readline())
    result = fibonacci(n)
    println(result)
end


main()
