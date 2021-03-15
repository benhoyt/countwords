function Split(table, str)
    for token in string.gmatch(str, "%S+") do
        token = string.lower(token)
        if table[token] == nil then
            table[token] = 0
        end
        table[token] = table[token] + 1
    end
    return table
end

function Read()
    local BUFSIZE = 2^13
    local f = io.input(arg[1])
    local a = {}
    while true do
        local lines, rest = f:read(BUFSIZE, "*line")
        if not lines then break end
        if rest then lines = lines .. rest .. '\n' end
        a = Split(a, lines)
    end
    local count = 1
    local tab = {}
    for key, value in pairs(a) do
        tab[count] = {str = key, val = value}
        count = count + 1
    end
    table.sort(tab, function (x, y) return x.val > y.val end)
    return tab
end

local a = Read()
for _, value in ipairs(a) do
    print(value.str, value.val)
end