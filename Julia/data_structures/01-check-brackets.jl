const openingbrackets = Set(['(', '[', '{'])
const closingbrackets = Set([')', ']', '}'])


match(opening, closing) = ((opening == '[' && closing == ']') ||
                           (opening == '(' && closing == ')') ||
                           (opening == '{' && closing == '}'))


function failedopeningindex(stack)
    if !isempty(stack)
        _, ix = pop!(stack)
        ix
    end
end


function indexofnonmatchingbracket(string)
    stack = NamedTuple{(:bracket, :position), Tuple{Char, Int}}[]
    for (ix, letter) in enumerate(string)
        if letter in openingbrackets
            push!(stack, (bracket=letter, position=ix))
        elseif letter in closingbrackets
            if isempty(stack)
                return ix
            end
            openingbracket, _ = pop!(stack)
            if !match(openingbracket, letter)
                return ix
            end
        end
    end
    failedopeningindex(stack)
end


function main()
    string = readline()
    failedindex = indexofnonmatchingbracket(string)
    if failedindex == nothing
        println("Success")
    else
        println(failedindex)
    end
end


main()
