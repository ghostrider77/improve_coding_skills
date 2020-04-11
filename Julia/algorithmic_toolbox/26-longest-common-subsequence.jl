function readinputdata()
    sequences = NamedTuple{(:seq, :length), Tuple{Vector{Int}, Int}}[]
    k = 0
    for k = 1:3
        ix = 2k
        size = parse(Int, readline())
        sequence = map(c -> parse(Int, c), split(readline()))
        push!(sequences, (seq=sequence, length=size))
    end
    sequences
end


function longestcommonsubsequence(data)
    s₁, s₂, s₃ = map(s -> s.seq, data)
    n₁, n₂, n₃ = map(s -> s.length, data)

    longestpath = zeros(Int, (n₁ + 1, n₂ + 1, n₃ + 1))
    for i in 1:n₁, j in 1:n₂, k in 1:n₃
        longestpath[i+1, j+1, k+1] = begin
            if s₁[i] == s₂[j] == s₃[k]
                longestpath[i, j, k] + 1
            else
                max(longestpath[i, j+1, k+1], longestpath[i+1, j, k+1], longestpath[i+1, j+1, k])
            end
        end
    end
    longestpath[n₁+1, n₂+1, n₃+1]
end


function main()
    data = readinputdata()
    result = longestcommonsubsequence(data)
    println(result)
end


main()
