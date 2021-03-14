
import subprocess
import time

NUM_RUNS = 5
INPUT_FILENAME = 'kjvbible_x10.txt'

def time_run(cmdline):
    cmdline = cmdline + ' <{} >/dev/null'.format(INPUT_FILENAME)
    times = []
    for _ in range(NUM_RUNS):
        start = time.time()
        subprocess.run(cmdline, shell=True)
        elapsed = time.time() - start
        times.append(elapsed)
    return min(times)

programs = [
    ('`grep foobar`', 'grep foobar', 'LC_ALL=C grep foobar', '`grep` reference; optimized sets `LC_ALL=C`'),
    ('`wc -w`', 'wc -w', 'LC_ALL=C wc -w', '`wc` reference; optimized sets `LC_ALL=C`'),
    ('C', './simple-c', './optimized-c', ''),
    ('Go', './simple-go', './optimized-go', ''),
    ('C++', './simple-cpp', './optimized-cpp', '"optimized" isn\'t very optimized'),
    ('Python', 'python3 simple.py', 'python3 optimized.py', ''),
    ('AWK', 'gawk -f simple.awk', 'mawk -f optimized.awk', 'optimized uses `mawk`'),
    ('Forth', '../gforth/gforth-fast simple.fs', '../gforth/gforth-fast optimized.fs', ''),
    ('Shell', 'bash simple.sh', 'bash optimized.sh', 'optimized sets `LC_ALL=C`'),
]

print('Language      | Simple | Optimized | Notes')
print('------------- | ------ | --------- |')
for lang, simple, optimized, notes in programs:
    simple_time = time_run(simple)
    if optimized:
        optimized_time = time_run(optimized)
        print('{:13} | {:6.2f} | {:9.2f} | {}'.format(lang, simple_time, optimized_time, notes))
    else:
        print('{:13} | {:6.2f} | {:9} | {}'.format(lang, simple_time, '', notes))
