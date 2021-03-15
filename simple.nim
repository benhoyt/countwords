import algorithm
import sequtils
import strutils
import tables

type Word = object
    word: string
    count: int

proc wordCmp(a, b: Word): int =
    if a.count < b.count:
        return 1
    if a.count > b.count:
        return -1
    return -cmpIgnoreCase(a.word, b.word)

when isMainModule:
    var line = ""
    var counts: Table[string, Word]
    while stdin.readLine(line):
        for word in splitWhitespace(toLowerAscii(line)):
            if not counts.haskey(word):
                counts[word] = Word(word: word, count: 0)
            counts[word].count += 1

    var countsList: seq[Word] = newSeq[Word](counts.len)
    for index, word in toSeq(counts.keys):
        countsList[index] = counts[word]
    countsList.sort(wordCmp)

    for word in countsList:
        echo(word.word, " ", word.count)
