#!/usr/bin/env python3

# Benchmark different versions and output results table as Markdown
# NOTE: run after ./test.sh (which compiles the binaries)

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
    ('`grep`', 'grep foobar', 'LC_ALL=C grep foobar', '`grep` baseline; optimized sets `LC_ALL=C`'),
    ('`wc -w`', 'wc -w', 'LC_ALL=C wc -w', '`wc` baseline; optimized sets `LC_ALL=C`'),
    ('C', './simple-c', './optimized-c', ''),
    ('Go', './simple-go', './optimized-go', ''),
    ('Rust A', './rust/simple/target/release/countwords', './rust/optimized/target/release/countwords', 'by Andrew Gallant'),
    ('Rust B', './rust/bonus/target/release/countwords', './rust/optimized-customhashmap/target/release/countwords', 'also by Andrew: bonus and custom hash'),
    ('C++', './simple-cpp', './optimized-cpp', 'optimized by Jussi Pakkanen'),
    ('Python', 'python3 simple.py', 'python3 optimized.py', ''),
    ('Ruby', 'ruby simple.rb', 'ruby optimized.rb', 'by Bill Mill'),
    ('C#', './simple-cs', None, 'original by John Taylor'),
    ('AWK', 'gawk -f simple.awk', 'mawk -f optimized.awk', 'optimized uses `mawk`'),
    ('Forth', '../gforth/gforth-fast simple.fs', '../gforth/gforth-fast optimized.fs', ''),
    ('Shell', 'bash simple.sh', 'bash optimized.sh', 'optimized does `LC_ALL=C sort -S 2G`'),
    ('Julia', 'julia simple.jl', None, 'by Alessandro Melis'),
    ('Perl', 'perl simple.pl', None, 'by Charles Randall'),
    ('JavaScript', 'node ./simple', 'node ./optimized', 'by Dani Biro'),
    ('Nim', './simple-nim', None, 'by csterritt and euantorano'),
    ('PHP', 'php simple.php', None, 'by Max Semenik'),
]

times = []
for program in programs:
    lang, simple, optimized, _ = program
    print('Timing', lang)
    simple_time = time_run(simple)
    optimized_time = time_run(optimized) if optimized else None
    times.append((program, simple_time, optimized_time))
times.sort(key=lambda x: x[1])  # sort by simple_time

print()
print('Language      | Simple | Optimized | Notes')
print('------------- | ------ | --------- | -----')
for program, simple_time, optimized_time in times:
    lang, _, _, notes = program
    if optimized_time is not None:
        print('{:13} | {:6.2f} | {:9.2f} | {}'.format(lang, simple_time, optimized_time, notes))
    else:
        print('{:13} | {:6.2f} | {:9} | {}'.format(lang, simple_time, '', notes))
