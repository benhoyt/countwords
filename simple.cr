counts = Hash(String, Int32).new(0)

STDIN.each_line do |line|
  line.downcase.split.each do |word|
    counts[word] += 1
  end
end

entries = counts.to_a.sort_by! &.[1]
entries.reverse_each do |(word, count)|
  puts "#{word} #{count}"
end
