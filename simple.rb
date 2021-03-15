# frozen_string_literal: true

word_count = {}

$stdin.each_line do |line|
  line.downcase.split.each do |word|
    word_count[word] = word_count[word].to_i + 1 # nil.to_i => 0
  end
end

word_count.sort_by { |_key, value| value }
          .each { |pair| puts "#{pair[0]}: #{pair[1]}" }
