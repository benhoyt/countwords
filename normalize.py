"""Normalize output so same-frequency words are ordered alphabetically.

For example, this output:

foo 4
bar 2
abc 2
zoo 1

Becomes:

foo 4
abc 2
bar 2
zoo 1
"""

import sys

def print_group(group):
    for word, count in sorted(group):
        print(word, count)

prev = None
group = []
for i, line in enumerate(sys.stdin, start=1):
    try:
        word, count = line.split()
    except ValueError:
        print('line {}, cannot split: {!r}'.format(i, line), file=sys.stderr)
        raise
    if count != prev:
        print_group(group)
        group = []
    group.append((word, count))
    prev = count

print_group(group)
