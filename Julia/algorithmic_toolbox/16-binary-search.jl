converttointlist(line) = map(x -> parse(Int, x), split(line))


function listandsize(line)
    list = converttointlist(line)
    list[2:end], list[1]
end


function findqueryelem(list, n, q)
    function search(left, right)
        if left > right
            return -1
        end
        ix = (left + right) ÷ 2
        elem = list[ix]
        if elem == q
            ix
        elseif elem < q
            search(ix + 1, right)
        else
            search(left, ix - 1)
        end
    end

    search(1, length(list))
end


function main()
    lines = readlines()
    list, n = listandsize(lines[1])
    queries, _ = listandsize(lines[2])
    result = map(q -> findqueryelem(list, n, q), queries)
    indexcorrected = map(ix -> ix == -1 ? -1 : ix - 1, result)
    println(join(indexcorrected, ' '))
end


main()
