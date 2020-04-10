largestfrompieces(numbers) = join(sort(numbers; lt=(n1, n2) -> n1 * n2 >= n2 * n1))


function main()
    _ = readline()
    numbers = readline() |> split
    result = largestfrompieces(numbers)
    println(result)
end


main()
