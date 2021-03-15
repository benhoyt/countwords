counts = Hash.new(0)
bufsize = 64*1024
remaining = ''
while 1 do
  chunk = STDIN.read(bufsize)
  if !chunk
    break
  end
  chunk = remaining + chunk
  last_lf = chunk.rindex("\n")
  if last_lf.nil?
    remaining = ''
  else
    remaining = chunk[last_lf+1..]
    chunk = chunk[..last_lf]
  end
  chunk.downcase.split.each{ |w| counts[w] += 1 }
end

counts.sort_by{|k,v| -v}.each{|k,v| puts "#{k} #{v}"}
