
Playing with counting word frequencies (and the performance thereof) in various languages. See the full article for context and background: [https://benhoyt.com/writings/count-words/](https://benhoyt.com/writings/count-words/)

To run the tests and benchmarks (you'll need Go, Rust, Python 3, AWK and perhaps other tools installed). Only tested on Linux:

```bash
./test.sh
./benchmark.py
```

Kudos and credits to Andrew Gallant for the Rust versions.

# Latest Result

| Language | Simple | Optimized |                  Notes                  |
|:--------:|:------:|:---------:|:---------------------------------------:|
| grep     | 0.04   | 0.04      | grep reference; optimized sets LC_ALL=C |
| wc -w    | 0.29   | 0.20      | wc reference; optimized sets LC_ALL=C   |
| C        | 0.97   | 0.23      |                                         |
| Go       | 1.14   | 0.38      |                                         |
| Rust A   | 1.41   | 0.35      | by Andrew Gallant                       |
| Rust B   | 1.48   | 0.28      | also by Andrew: bonus and custom hash   |
| C++      | 1.75   | 0.98      | “optimized” isn’t very optimized        |
| Python   | 2.07   | 1.27      |                                         |
| C#       | 3.43   |           | original by John Taylor                 |
| AWK      | 3.52   | 1.13      | optimized uses mawk                     |
| Forth    | 4.21   | 1.44      |                                         |
| Shell    | 14.67  | 1.86      | optimized does LC_ALL=C sort -S 2G      |
