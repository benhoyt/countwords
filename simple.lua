-- Written by [https://github.com/catwell](catwell) and modified by DarkWarrior703
local counts = {}
local t = {}

local function insert(s)
    counts[s] = (counts[s] or 0) + 1
end
-- Read the whole buffer, lower it and then create the count table
local lines = io.lines()
for line in lines do
    string.gsub(line:lower(), "(%S+)", insert)    
end

for k in pairs(counts) do
    table.insert(t, k)
end

table.sort(t, function(a, b) return counts[a] > counts[b] end)

-- Print the word and his count
for _, v in ipairs(t) do
    print(v .. ' ' .. counts[v])
end