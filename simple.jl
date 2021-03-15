function wc(path, counter=Dict())
    for line in eachline(path), word in split(line)
        counter[word] = get(counter, word, 0) + 1
    end
    counter
end

sortdict(counter) = sort(collect(counter), by=x->x[2], rev=true)

wc(ARGS[1]) |> sortdict .|> kv->println(kv[1], " ", kv[2]) 
