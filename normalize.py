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
for line in sys.stdin:
    try:
        word, count = line.split()
    except ValueError:
        print('cannot split: {!r}'.format(line), file=sys.stderr)
        raise
    if count != prev:
        print_group(group)
        group = []
    group.append((word, count))
    prev = count

print_group(group)
