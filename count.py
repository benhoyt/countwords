
import sys

counts = {}
for line in sys.stdin:
    line = line.lower()
    for word in line.split():
#        word = word.lower()
        counts[word] = counts.get(word, 0) + 1

lst = sorted(counts.items(), key=lambda kv: kv[1], reverse=True)
for word, count in lst:
    print(word, count)



# from collections import Counter
# import sys
# for w, c in Counter(sys.stdin.buffer.read().lower().split()).most_common():
#     print(w, c)


"""
   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
     5437    0.004    0.000    0.009    0.000 codecs.py:319(decode)
    32711    0.002    0.000    0.002    0.000 count.py:11(<lambda>)
        1    2.220    2.220    3.573    3.573 count.py:2(<module>)
     5437    0.005    0.000    0.005    0.000 {built-in method _codecs.utf_8_decode}
        1    0.000    0.000    3.573    3.573 {built-in method builtins.exec}
    32711    0.021    0.000    0.021    0.000 {built-in method builtins.print}
        1    0.008    0.008    0.010    0.010 {built-in method builtins.sorted}
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
  8241920    0.816    0.000    0.816    0.000 {method 'get' of 'dict' objects}
        1    0.000    0.000    0.000    0.000 {method 'items' of 'dict' objects}
  1002310    0.103    0.000    0.103    0.000 {method 'lower' of 'str' objects}
  1002310    0.395    0.000    0.395    0.000 {method 'split' of 'str' objects}
"""