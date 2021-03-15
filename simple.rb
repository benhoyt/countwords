# frozen_string_literal: true

word_count = Hash.new(0)

$stdin.each_line do |line|
  line.downcase.split.each do |word|
    word_count[word] += 1
  end
end

word_count.sort_by { |_key, value| value }
          .each { |pair| puts "#{pair[0]}: #{pair[1]}" }
