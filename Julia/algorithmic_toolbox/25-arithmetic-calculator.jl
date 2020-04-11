const mapping = Dict('+' => +, '-' => -, '*' => *)


function readinputdata(line)
    digits = [parse(Int, c) for c in line[1:2:end]]
    operations = line[2:2:end]
    digits, operations
end


function calcminmax(ix, jy, operations, minimumofsubexpressions, maximumofsubexpressions)
    subexpressionmin = typemax(Int)
    subexpressionmax = typemin(Int)
    for k in ix:(jy-1)
        op = mapping[operations[k]]
        a = op(maximumofsubexpressions[ix, k], maximumofsubexpressions[k+1, jy])
        b = op(maximumofsubexpressions[ix, k], minimumofsubexpressions[k+1, jy])
        c = op(minimumofsubexpressions[ix, k], maximumofsubexpressions[k+1, jy])
        d = op(minimumofsubexpressions[ix, k], minimumofsubexpressions[k+1, jy])
        subexpressionmin = min(subexpressionmin, a, b, c, d)
        subexpressionmax = max(subexpressionmax, a, b, c, d)
    end
    subexpressionmin, subexpressionmax
end


function maximizearithmeticexpression(digits, operations)
    n = length(digits)
    minimumofsubexpressions = zeros(Int, (n, n))
    maximumofsubexpressions = zeros(Int, (n, n))
    for (ix, digit) in enumerate(digits)
        minimumofsubexpressions[ix, ix] = digit
        maximumofsubexpressions[ix, ix] = digit
    end

    for s in 1:(n-1), ix in 1:(n-s)
        jy = ix + s
        submin, submax = calcminmax(ix, jy, operations, minimumofsubexpressions, maximumofsubexpressions)
        minimumofsubexpressions[ix, jy] = submin
        maximumofsubexpressions[ix, jy] = submax
    end
    maximumofsubexpressions[1, n]
end


function main()
    digits, operations = readline() |> readinputdata
    result = maximizearithmeticexpression(digits, operations)
    println(result)
end


main()
