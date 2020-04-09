const coins = (10, 5, 1)


function minimumnumberofchanges(amount)
    nrchanges = 0
    for coin in coins
        nrchanges += (amount ÷ coin)
        amount = amount % coin
    end
    nrchanges
end


function main()
    amount = parse(Int, readline())
    result = minimumnumberofchanges(amount)
    println(result)
end


main()
