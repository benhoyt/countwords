function wc(io, counter=Dict{String,Int}())
    for line in eachline(io), word in split(line)
        lword = lowercase(word)
        counter[lword] = get(counter, lword, 0) + 1
    end
    counter
end

sortdict(counter) = sort!(collect(counter), by=x->x[2], rev=true)

wc(stdin) |> sortdict .|> kv->println(kv[1], " ", kv[2]) 
