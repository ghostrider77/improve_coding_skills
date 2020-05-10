using Random


polynomialhash(s, prime, x) = foldr((char, acc) -> (acc*x + Int(char)) % prime, s; init=0)

powerofX(x, prime, k) = foldl((acc, _) -> acc*x % prime, 1:k; init=1)


function precompute_substring_hashes(text, textlength, patternlength, p, x)
    hashes = zeros(Int, textlength-patternlength+1)
    lastsubstring = text[end-patternlength+1:end]
    hashes[end] = polynomialhash(lastsubstring, p, x)
    xpower = powerofX(x, p, patternlength)
    for ix in (textlength-patternlength):-1:1
        hashes[ix] = (x*hashes[ix+1] + Int(text[ix]) - xpower*Int(text[ix+patternlength])) % p
    end
    hashes
end


function rabinkarp(text, pattern, prime)
    x = Random.rand(1:(prime-1))
    patternlength = length(pattern)
    textlength = length(text)
    matched = Int[]
    h = polynomialhash(pattern, prime, x)
    substringhashes = precompute_substring_hashes(text, textlength, patternlength, prime, x)
    for ix = 1:(textlength-patternlength+1)
        if h == substringhashes[ix] && pattern == text[ix:ix+patternlength-1]
            push!(matched, ix - 1)
        end
    end
    matched
end


function main()
    pattern = readline() |> collect
    text = readline() |> collect
    prime = 1000000007
    Random.seed!(2112)
    result = rabinkarp(text, pattern, prime)
    println(join(result, " "))
end


main()
