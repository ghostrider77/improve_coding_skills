function levenshtein(s₁, s₂)
    n = length(s₁)
    m = length(s₂)
    editdistance = zeros(Int, (n+1, m+1))
    editdistance[:, 1] = 0:n
    editdistance[1, :] = 0:m
    for (ix, c₁) in enumerate(s₁), (jy, c₂) in enumerate(s₂)
        deletion = editdistance[ix, jy+1] + 1
        insertion = editdistance[ix+1, jy] + 1
        match = editdistance[ix, jy] + (c₁ == c₂ ? 0 : 1)
        editdistance[ix+1, jy+1] = min(deletion, insertion, match)
    end
    editdistance[n+1, m+1]
end


function main()
    s₁ = readline()
    s₂ = readline()
    dist = levenshtein(s₁, s₂)
    println(dist)
end


main()
