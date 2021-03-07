import collections
import sys

counts = collections.Counter()
remaining = ''
while True:
    chunk = sys.stdin.read(64*1024)
    if not chunk:
        break
    chunk = remaining + chunk
    last_lf = chunk.rfind('\n')
    if last_lf == -1:
        remaining = ''
    else:
        remaining = chunk[last_lf+1:]
        chunk = chunk[:last_lf]
    counts.update(chunk.lower().split())

for word, count in counts.most_common():
    print(word, count)
