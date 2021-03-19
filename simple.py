import collections
import sys

for word, count in collections.Counter(
    word
    for line in sys.stdin
    for word in line.lower().split()
).most_common():
    print(word, count)
