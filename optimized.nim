# inspired by simple.nim and optimized.go

from tables import initCountTable, inc, sort, pairs
from algorithm import SortOrder
from strutils import rfind

proc main() =

  var table = initCountTable[string]()

  const chunkSize = 64 * 1024
  var chunk = newString(chunkSize)

  var offset = 0
  while true:
    # Read input in 64KB blocks
    let n = stdin.readBuffer(chunk[offset].addr, chunkSize - offset)
    if n == 0:
      break

    # Find last end-of-line character in block read.
    var last = offset + n - 1
    let lastLF = chunk.rfind('\n', 0, last)
    if lastLF != 1:
      last = lastLF - 1

    var start = -1 # // start -1 means in whitespace run
    for i in 0..last:
      var c = chunk[i]
      # Convert to ASCII lowercase in place as we go.
      if c >= 'A' and c <= 'Z':
        chunk[i] = chr(ord(c) + (ord('a') - ord('A')))
      if start >= 0:
        # In a word, look for end of word (whitespace).
        if c <= ' ':
          # Count this word!
          table.inc(chunk[start..<i])
          start = -1
      else:
        # In whitespace, look for start of word (non-space).
        if c > ' ':
          start = i

    # Count last word, if any.
    if start >= 0 and start <= last:
      table.inc(chunk[start..last])

    # Copy remaining bytes (incomplete line) to start of buffer.
    if lastLF != -1 and lastLF != chunkSize - 1:
      let remainLen = chunkSize - lastLF - 1
      var remaining = chunk[lastLF+1..^1]
      copyMem(chunk[0].addr, remaining[0].addr, remainLen)
      offset = remainLen
    else:
      offset = 0

  table.sort(SortOrder.Descending)

  for k, v in table.pairs():
    echo(k, ' ', v)

when isMainModule:
  main()
