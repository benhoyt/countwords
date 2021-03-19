#!usr/bin/env lua-5.4

local words = {}

for line in io.stdin:lines() do
   if line:match("[^%s]") then
      for word in line:gmatch("%s*([^%s]*)%s*") do
         if #word > 0 then
            word = word:lower()
            words[word] = (words[word] or 0) + 1
         end
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
