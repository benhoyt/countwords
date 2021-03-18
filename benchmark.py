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
    ('Rust', './rust/simple/target/release/countwords', './rust/optimized/target/release/countwords', 'by Andrew Gallant'),
    ('C++', './simple-cpp', './optimized-cpp', 'optimized by Jussi P, Adev, Nathan M'),
    ('Python', 'python3 simple.py', 'python3 optimized.py', ''),
    ('Ruby', 'ruby simple.rb', 'ruby optimized.rb', 'by Bill Mill'),
    ('C#', './csharp/simple/bin/Release/net5.0/simple', './csharp/optimized/bin/Release/net5.0/optimized', 'by J Taylor, Y Ostapenko, O Turan'),
    ('F#', './fsharp/simple/bin/Release/net5.0/simple', './fsharp/optimized/bin/Release/net5.0/optimized', 'by Yuriy Ostapenko'),
    ('AWK', 'gawk -f simple.awk', 'mawk -f optimized.awk', 'optimized uses `mawk`'),
    ('Forth', '../gforth/gforth-fast simple.fs', '../gforth/gforth-fast optimized.fs', ''),
    ('Shell', 'bash simple.sh', 'bash optimized.sh', 'optimized does `LC_ALL=C sort -S 2G`'),
    ('Crystal', './simple-cr', None, 'by Andrea Manzini'),
    ('Swift', './simple-swift', None, 'by Daniel Muellenborn'),
    ('Perl', 'perl simple.pl', None, 'by Charles Randall'),
    ('JavaScript', 'node ./simple', 'node ./optimized', 'by Dani Biro and Flo Hinze'),
    ('Nim', './simple-nim', './optimized-nim', 'by csterritt and euantorano'),
    ('PHP', 'php simple.php', None, 'by Max Semenik'),
    ('D', './simple-d', './optimized-d', 'by Ross Lonstein'),
    ('OCaml', './simple-ml', None, 'by Nate Dobbins and Pavlo Khrystenko'),
    ('Kotlin', 'java -jar simple-kotlin.jar', None, 'by Kazik Pogoda'),
    ('Lua', 'luajit simple.lua', 'luajit optimized.lua', 'by themadsens; runs under luajit'),
    ('Zig', './simple-zig', None, 'by ifreund and matu3ba'),
]

times = []
for program in programs:
    lang, simple, optimized, _ = program
    print('Timing', lang, end=' ', flush=True)
    simple_time = time_run(simple)
    optimized_time = time_run(optimized) if optimized else None
    print('{:.2f} {:.2f}'.format(simple_time, optimized_time or 0))
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
