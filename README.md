
Playing with counting word frequencies (and the performance thereof) in various languages. See the full article for context and background: [https://benhoyt.com/writings/count-words/](https://benhoyt.com/writings/count-words/)


## NOTE

I received dozens of pull requests when my [count-words article](https://benhoyt.com/writings/count-words/) first came out, almost to the point where I couldn't keep up with them. Thanks for all the submissions!

However, I'm **not taking new submissions anymore,** due mainly to the amount of time I've spent on this already. The major languages are there, and many minor ones. However, I'm still open to receiving feedback -- you can either email me (via the email address on my website) or open an issue here. Thanks!


## To run the benchmarks

To run the tests and benchmarks (you'll need Go, Rust, Python 3, AWK and perhaps other tools installed). Only tested on Linux:

```bash
./test.sh
./benchmark.py
```

For the latest results (run on my machine against these [versions](https://github.com/benhoyt/countwords/blob/master/versions.txt)), see the article's [performance results section](https://benhoyt.com/writings/count-words/#performance-results-and-learnings).


## Credits

Thanks to these contributors for additional language versions:

* Bash: [Jesse Hathaway](https://github.com/lollipopman) - not included in benchmarks as it takes over 2 minutes
* C#: [John Taylor](https://github.com/jftuga), [Yuriy Ostapenko](https://github.com/uncleyo) and [Osman Turan](https://github.com/osman-turan)
* C# (LINQ): [Osman Turan](https://github.com/osman-turan) - not run in benchmarks
* C++ optimized version: [Jussi Pakkanen](https://github.com/jpakkane), [Adev](https://github.com/adevress), [Nathan Myers](https://github.com/ncm)
* Common Lisp: [Brad Svercl](https://github.com/bradms)
* Crystal: [Andrea Manzini](https://github.com/ilmanzo)
* D: [Ross Lonstein](https://github.com/rlonstein)
* F#: [Yuriy Ostapenko](https://github.com/uncleyo)
* Go: [Miguel Angel](https://github.com/ntrrg) - simplifying the Go optimized version; [Joshua Corbin](https://github.com/jcorbin) - adding a [parallel Go version](https://github.com/benhoyt/countwords/blob/9db2ab6808921e649fc5212c00712e61edf6fa1c/parallel.go) for demonstration
* Haskell: [Adrien Glauser](https://github.com/why-not-try-calmer)
* Java: [Iulian Pleșoianu](https://github.com/bit-twit)
* JavaScript: [Dani Biró](https://github.com/Daninet) and [Flo Hinze](https://github.com/laubsauger)
* Julia: [Alessandro Melis](https://github.com/alemelis)
* Kotlin: [Kazik Pogoda](https://github.com/morisil)
* Lua: [Flemming Madsen](https://github.com/themadsens)
* Nim: [csterritt](https://github.com/csterritt) and [euantorano](https://github.com/euantorano)
* OCaml: [doesntgolf](https://github.com/doesntgolf)
* Pascal: [Osman Turan](https://github.com/osman-turan)
* Perl: [Charles Randall](https://github.com/charles-randall)
* PHP: [Max Semenik](https://github.com/MaxSem)
* Ruby: [Bill Mill](https://github.com/llimllib), with input from [Niklas](https://github.com/nhh)
* Rust: [Andrew Gallant](https://github.com/BurntSushi)
* Swift: [Daniel Müllenborn](https://github.com/damuellen)
* Tcl: [William Ross](https://github.com/arideden)
* Zig: [ifreund](https://github.com/ifreund) and [matu3ba](https://github.com/matu3ba) and [ansingh](https://github.com/ansingh)

See other versions [on Rosetta Code](https://rosettacode.org/wiki/Word_frequency).
