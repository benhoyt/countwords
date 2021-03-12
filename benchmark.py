
import subprocess
import time

programs = [
    ('Python', 'python3 simple.py', 'python3 optimized.py'),
    ('Go', './simple-go', './optimized-go'),
    ('C++', './simple-cpp', './optimized-cpp'),
    ('C', './simple-c', './optimized-c'),
    ('AWK', 'gawk -f simple.awk', 'mawk -f optimized.awk'),
    ('Forth', '../gforth/gforth simple.fs', None),
    ('Shell', 'bash simple.sh', None),
]

def time_run(cmdline):
    cmdline = cmdline + ' <kjvbible_x10.txt >/dev/null'
    times = []
    for _ in range(5):
        start = time.time()
        subprocess.run(cmdline, shell=True, check=True)
        elapsed = time.time() - start
        times.append(elapsed)
    return min(times)

print('Language | Simple | Optimized')
print('-------- | ------ | ---------')

for lang, simple, optimized in programs:
    simple_time = time_run(simple)
    if optimized:
        optimized_time = time_run(optimized)
        print('{:8} | {:6.2f} | {:9.2f}'.format(lang, simple_time, optimized_time))
    else:
        print('{:8} | {:6.2f} | {:9}'.format(lang, simple_time, ''))
