struct Query
    type::String
    number::Int
    name::Union{String, Nothing}

    Query(type::String, number::String, name::String) = new(type, parse(Int, number), name)
    Query(type::String, number::String) = new(type, parse(Int, number), nothing)
end


struct PhoneBook
    book::Vector{Union{String, Nothing}}

    function PhoneBook(size::Int)
        book = Union{String, Nothing}[]
        for _ = 1:size
            push!(book, nothing)
        end
        new(book)
    end
end


add!(phonebook::PhoneBook, number::Int, name::String) = phonebook.book[number+1] = name


delete!(phonebook::PhoneBook, number::Int) = phonebook.book[number+1] = nothing


find(phonebook::PhoneBook, number::Int) = phonebook.book[number+1]


function readqueries(n)
    queries = Query[]
    for _ in 1:n
        line = map(s -> String(s), split(readline()))
        push!(queries, Query(line...))
    end
    queries
end


function processqueries(queries, maxsize)
    phonebook = PhoneBook(maxsize)
    result = String[]
    for query in queries
        operation = query.type
        if operation == "add"
            add!(phonebook, query.number, query.name)
        elseif operation == "del"
            delete!(phonebook, query.number)
        else
            res = find(phonebook, query.number)
            if res == nothing
                push!(result, "not found")
            else
                push!(result, res)
            end
        end
    end
    result
end


function main()
    nrqueries = parse(Int, readline())
    queries = readqueries(nrqueries)
    maxsize = 10000000
    result = processqueries(queries, maxsize)
    for item in result
        println(item)
    end
end


main()
