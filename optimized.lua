#!usr/bin/env lua-5.4

local words = {}
local rest = ""

local function chunks(fd)
   return function() return fd:read(64*1024) end
end

for chunk in chunks(io.stdin) do
   local ix = string.find(string.reverse(chunk), "\n", 1, true) or 0
   if ix == 1 then
      chunk, rest = rest..chunk, ""
   else
      chunk, rest = rest..chunk:sub(1, -ix), chunk:sub(1 - ix)
   end
   for word in chunk:gmatch("%s*([^%s]*)%s*") do
      if #word > 0 then
         word = word:lower()
         words[word] = (words[word] or 0) + 1
      end
   end
end

local wordlist = {}
for word,_ in pairs(words) do
   table.insert(wordlist, word)
end
table.sort(wordlist, function(a,b) return words[a] > words[b] end)

for _,word in ipairs(wordlist) do
   print(word .. " " .. words[word])
end
