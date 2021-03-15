function wc(lines, counter=Dict())
    for line in lines, word in split(line)
        get!(counter, lowercase(word), 0) + 1
    end
    counter
end

sortdict(counter) = sort(collect(counter), by=x->x[2], rev=true)

wc(readlines()) |> sortdict .|> kv->println(kv[1], " ", kv[2]) 
