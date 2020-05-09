struct Query
    type::String
    value::Union{String, Nothing}
    k::Union{Int, Nothing}

    Query(type::String, value::String) = new(type, value, nothing)
    Query(type::String, k::Int) = new(type, nothing, k)
end


struct HashTable
    prime::Int
    x::Int
    cardinality::Int
    table::Vector{Vector{String}}

    function HashTable(prime::Int, x::Int, cardinality::Int)
        table = map(_ -> String[], 1:cardinality)
        new(prime, x, cardinality, table)
    end
end


function polynomialhash(hashtable::HashTable, s::String)
    foldr((char, acc) -> (acc*hashtable.x + Int(char)) % hashtable.prime, s; init=0) % hashtable.cardinality
end


function add!(hashtable::HashTable, s::String)
    hashvalue = polynomialhash(hashtable, s)
    chain = hashtable.table[hashvalue+1]
    if s ∉ chain
        push!(chain, s)
    end
end


function delete!(hashtable::HashTable, s::String)
    hashvalue = polynomialhash(hashtable, s)
    chain = hashtable.table[hashvalue+1]
    ix = findfirst(x -> x == s, chain)
    if ix != nothing
        deleteat!(chain, ix)
    end
end


function find(hashtable::HashTable, s::String)
    hashvalue = polynomialhash(hashtable, s)
    chain = hashtable.table[hashvalue+1]
    s ∈ chain ? "yes" : "no"
end


check(hashtable::HashTable, k::Int) = join(reverse(hashtable.table[k+1]), " ")


function readqueries(n)
    queries = Query[]
    for _ in 1:n
        a, b = map(String, split(readline()))
        query = a == "check" ? Query(a, parse(Int, b)) : Query(a, b)
        push!(queries, query)
    end
    queries
end


function processqueries(queries, cardinality, prime, x)
    hashtable = HashTable(prime, x, cardinality)
    results = String[]
    for query in queries
        operation = query.type
        if operation == "add"
            add!(hashtable, query.value)
        elseif operation == "del"
            delete!(hashtable, query.value)
        elseif operation == "find"
            push!(results, find(hashtable, query.value))
        else
            push!(results, check(hashtable, query.k))
        end
    end
    results
end


function main()
    cardinality = parse(Int, readline())
    nrqueries = parse(Int, readline())
    queries = readqueries(nrqueries)
    prime = 1000000007
    x = 263
    result = processqueries(queries, cardinality, prime, x)
    for item in result
        println(item)
    end
end


main()
