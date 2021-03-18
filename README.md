
Playing with counting word frequencies (and the performance thereof) in various languages. See the full article for context and background: [https://benhoyt.com/writings/count-words/](https://benhoyt.com/writings/count-words/)

To run the tests and benchmarks (you'll need Go, Rust, Python 3, AWK and perhaps other tools installed). Only tested on Linux:

```bash
./test.sh
./benchmark.py
```

For the latest results (run on my machine), see the article's [performance results section](https://benhoyt.com/writings/count-words/#performance-results-and-learnings).

Thanks to these contributors for additional language versions:

* Bash: [Jesse Hathaway](https://github.com/lollipopman) - not included in benchmarks as it takes over 2 minutes
* C#: [John Taylor](https://github.com/jftuga), [Yuriy Ostapenko](https://github.com/uncleyo) and [Osman Turan](https://github.com/osman-turan)
* C# (LINQ): [Osman Turan](https://github.com/osman-turan) - not run in benchmarks
* C++ optimized version: [Jussi Pakkanen](https://github.com/jpakkane), [Adev](https://github.com/adevress), [Nathan Myers](https://github.com/ncm)
* Crystal: [Andrea Manzini](https://github.com/ilmanzo)
* D: [Ross Lonstein](https://github.com/rlonstein)
* F#: [Yuriy Ostapenko](https://github.com/uncleyo)
* Java: [Iulian Pleșoianu](https://github.com/bit-twit)
* JavaScript: [Dani Biró](https://github.com/Daninet) and [Flo Hinze](https://github.com/laubsauger)
* Julia: [Alessandro Melis](https://github.com/alemelis)
* Kotlin: [Kazik Pogoda](https://github.com/morisil)
* Lua: [Flemming Madsen](https://github.com/themadsens)
* Nim: [csterritt](https://github.com/csterritt) and [euantorano](https://github.com/euantorano)
* OCaml: [doesntgolf](https://github.com/doesntgolf)
* Perl: [Charles Randall](https://github.com/charles-randall)
* PHP: [Max Semenik](https://github.com/MaxSem)
* Ruby: [Bill Mill](https://github.com/llimllib), with input from [Niklas](https://github.com/nhh)
* Rust: [Andrew Gallant](https://github.com/BurntSushi)
* Swift: [Daniel Müllenborn](https://github.com/damuellen)
* Zig: [ifreund](https://github.com/ifreund) and [matu3ba](https://github.com/matu3ba)

See other versions [on Rosetta Code](https://rosettacode.org/wiki/Word_frequency).
