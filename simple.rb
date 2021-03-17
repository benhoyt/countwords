#!/usr/bin/env ruby
counts = Hash.new(0)
STDIN.each_line { |line| line.downcase.split.each { |w| counts[w] += 1 }}
counts.sort_by { |k,v| -v }.each { |k,v| puts "#{k} #{v}" }
