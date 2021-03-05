import collections
import sys

counts = collections.Counter()
for line in sys.stdin:
    words = line.lower().split()
    counts.update(words)

for word, count in counts.most_common():
    print(word, count)
