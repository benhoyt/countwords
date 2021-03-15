import tables, strutils

when isMainModule:
  var wordFreqs: CountTable[string]
  for line in stdin.lines():
    for word in line.splitWhitespace():
      wordFreqs.inc(toLowerAscii(word))
  wordFreqs.sort()
  for (word, count) in wordFreqs.pairs():
    echo(word, " ", count)
