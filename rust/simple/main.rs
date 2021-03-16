// This version is roughly how I would write this program if someone gave
// me the problem as an interview challenge and said, "don't worry about
// performance." One of the main performance problems in this program is
// the `word.to_lowercase()` function, which results in an allocation. (It
// also does Unicode case folding, which is unnecessary for this particular
// challenge. But the simple Go variant does the same, and this is what I
// would reach for naturally, so I left it in.) However, one needs to allocate
// anyway to use the `entry` API, even though the common case is that the
// word already exists in the hash map. The code changes required to use
// `make_ascii_uppercase` and check if the word is already in the hash map (at
// the expense of doing another lookup if it isn't) are quite small and lead to
// a massive performance improvement. Nevertheless, this code is simpler.
//
// Other costs, although smaller, are UTF-8 validation and the fact that we're
// also allocating a new String for each line. (And the fact that we're even
// worrying about lines at all. The other Rust programs do not.)

use std::{
    collections::HashMap,
    error::Error,
    io::{self, BufRead, Write},
};

fn main() {
    // We don't return Result from main because it prints the debug
    // representation of the error. The code below prints the "display" or
    // human readable representation of the error.
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn try_main() -> Result<(), Box<dyn Error>> {
    let mut counts: HashMap<String, u64> = HashMap::new();
    for line in io::stdin().lock().lines() {
        for word in line?.split_whitespace() {
            *counts.entry(word.to_lowercase()).or_insert(0) += 1;
        }
    }

    let mut ordered: Vec<_> = counts.into_iter().collect();
    ordered.sort_unstable_by_key(|&(_, count)| count);

    for (word, count) in ordered.into_iter().rev() {
        writeln!(io::stdout(), "{} {}", word, count)?;
    }
    Ok(())
}
