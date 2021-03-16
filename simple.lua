<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> f37d2eb791ab9db670dc1dbabd007faad09baf2e
-- Written by [https://github.com/catwell](catwell) and modified by DarkWarrior703
local counts = {}
local t = {}

local function insert(s)
    counts[s] = (counts[s] or 0) + 1
end
-- Read the whole buffer, lower it and then create the count table
string.gsub(io.read("*all"):lower(), "(%S+)", insert)

for k in pairs(counts) do
    table.insert(t, k)
end

table.sort(t, function(a, b) return counts[a] > counts[b] end)

-- Print the word and his count
for _, v in ipairs(t) do
    print(v .. ' ' .. counts[v])
<<<<<<< HEAD
=======
=======
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
>>>>>>> 83fbd4c66fc90210160de771c7d2ace732cafbbe
>>>>>>> f37d2eb791ab9db670dc1dbabd007faad09baf2e
end