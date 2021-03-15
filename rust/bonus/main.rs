// This is the Rust program I would write if someone asked for a *versatile*
// program to find the frequency distribution of all the words, regardless of
// case, in a stream. This handles any file that is at least conventionally
// UTF-8 (so latin-1 is fine). It also uses the Unicode-aware word segmentation
// algorithm and Unicode-aware case folding. This is despite NOT using Rust's
// default Unicode string, since that requires the data to be valid UTF-8.
//
// It runs about as fast as the "simple" Rust and Go variants. We do two perf
// tricks in this program: 1) we use bstr's `to_lowercase_into` to avoid
// allocating a new string for every word, and 2) we look for an existing word
// in our hashmap to avoid an allocation via the more idiomatic 'entry' API.
// (These combined reflects a ~33% improvement in my ad hoc experiments.)
// According to profiling, a bit over half the runtime is spent in word
// segmenting. The rest seems somewhat evenly split between the actual
// lowercasing and hashmap interactions.
//
// The gauntlet has been laid down. How hard is to port this program to other
// languages? And when you do, what does its performance look like?

use std::io::{self, Write};

use bstr::{io::BufReadExt, BStr, BString, ByteSlice};
use fxhash::{FxHashMap as HashMap};

fn main() {
    // Rust blocks the broken pipe signal by default, and instead returns it as
    // an error from `write` if the consumer hangs up. So we look for it here
    // and exit gracefully, as an end user would expect.
    if let Err(err) = try_main() {
        if let Some(ioerr) = err.root_cause().downcast_ref::<io::Error>() {
            if ioerr.kind() == io::ErrorKind::BrokenPipe {
                std::process::exit(0);
            }
        }
        eprintln!("{:?}", err);
    }
}

fn try_main() -> anyhow::Result<()> {
    let stdin = io::stdin();
    let stdin = stdin.lock();

    let mut counts: HashMap<BString, u64> = HashMap::default();
    let mut buf = BString::from(vec![]);
    stdin.for_byte_line(|line| {
        for word in line.words() {
            // reuse the same buffer for lowercasing---an API not available
            // in std!---to avoid an alloc for every word.
            buf.clear();
            word.as_bytes().to_lowercase_into(&mut buf);
            increment(&mut counts, buf.as_bstr());
        }
        Ok(true)
    })?;

    let mut ordered: Vec<(BString, u64)> = counts.into_iter().collect();
    ordered.sort_by(|&(_, cnt1), &(_, cnt2)| cnt1.cmp(&cnt2).reverse());
    for (word, count) in ordered {
        writeln!(io::stdout(), "{} {}", word, count)?;
    }
    Ok(())
}

fn increment(counts: &mut HashMap<BString, u64>, word: &BStr) {
    // While this will do two hash lookups when 'word' is not in the map, it
    // will only do one lookup and no allocs in the much more common case of
    // 'word' being in the map.
    if let Some(count) = counts.get_mut(word) {
        *count += 1;
    } else {
        counts.insert(BString::from(word), 1);
    }
}
