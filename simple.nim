from tables import newCountTable, inc, sort, pairs
from algorithm import SortOrder
from strutils import split, toLowerAscii

proc main() =
  var table = newCountTable[string]()

  for line in stdin.lines:
    for word in line.toLowerAscii().split(' '):
      if word.len() == 0: continue
      table.inc(word)

  table.sort(SortOrder.Descending)

  for k, v in table.pairs():
    echo k, ' ', v

when isMainModule:
  main()
