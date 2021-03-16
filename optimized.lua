-- Written by [https://github.com/DarkWarrior703](DarkWarrior703)
-- It's 0.3 seconds faster than the simple version on my system
local BUFSIZE = 2^14
local a = {}
local tab = {}
local count = 1

while true do
    -- Read 2^14 bytes and then the rest of line
    local lines, rest = io.read(BUFSIZE, "*lines")
    if not lines then break end
    -- Concatenate the buffer and the rest of line
    if rest then lines = lines .. rest .. '\n' end
    lines = lines:lower()
    -- Find strings in lines
    for token in string.gmatch(lines, "%S+") do
        a[token] = (a[token] or 0) + 1
    end
end

-- Adding items through counting is faster than table.insert
for key, _ in pairs(a) do
    tab[count] = key
    count = count + 1
end

table.sort(tab, function(i, j) return a[i] > a[j] end)

-- Print the word and his count
for i, _ in ipairs(tab) do
    print(tab[i] .. ' ' .. a[tab[i]])
end