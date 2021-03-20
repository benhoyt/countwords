// This version is an approximate port of the optimized Go program. Its buffer
// handling is slightly simpler: we don't bother with dealing with the last
// newline character. (This may appear to save work, but it only saves work
// once per 64KB buffer, so is likely negligible. It's just simpler IMO.)
//
// There's nothing particularly interesting here other than swapping out std's
// default hashing algorithm for one that isn't cryptographically secure.

use std::{
    error::Error,
    io::{self, Read, Write},
};

// std uses a cryptographically secure hashing algorithm by default, which is
// a bit slower. In this particular program, fxhash and fnv seem to perform
// similarly, with fxhash being a touch faster in my ad hoc benchmarks. If
// we wanted to really enforce the "no external crate" rule, we could just
// hand-roll an fnv hash impl ourselves very easily.
//
// N.B. This crate brings in a new hashing function. We still use std's hashmap
// implementation.
use fxhash::FxHashMap as HashMap;

fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn try_main() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut counts: HashMap<Vec<u8>, u64> = HashMap::default();
    let mut buf = vec![0; 64 * (1 << 10)];
    let mut offset = 0;
    let mut start = None;
    loop {
        let nread = stdin.read(&mut buf[offset..])?;
        if nread == 0 {
            if offset > 0 {
                increment(&mut counts, &buf[..offset + 1]);
            }
            break;
        }
        let buf = &mut buf[..offset + nread];

        for i in (0..buf.len()).skip(offset) {
            let b = buf[i];
            if b'A' <= b && b <= b'Z' {
                buf[i] += b'a' - b'A';
            }
            if b == b' ' || b == b'\n' {
                if let Some(start) = start.take() {
                    increment(&mut counts, &buf[start..i]);
                }
            } else if start.is_none() {
                start = Some(i);
            }
        }
        if let Some(ref mut start) = start {
            offset = buf.len() - *start;
            buf.copy_within(*start.., 0);
            *start = 0;
        } else {
            offset = 0;
        }
    }

    let mut ordered: Vec<_> = counts.into_iter().collect();
    ordered.sort_unstable_by_key(|&(_, count)| count);

    for (word, count) in ordered.into_iter().rev() {
        writeln!(io::stdout(), "{} {}", std::str::from_utf8(&word)?, count)?;
    }
    Ok(())
}

fn increment(counts: &mut HashMap<Vec<u8>, u64>, word: &[u8]) {
    // using 'counts.entry' would be more idiomatic here, but doing so requires
    // allocating a new Vec<u8> because of its API. Instead, we do two hash
    // lookups, but in the exceptionally common case (we see a word we've
    // already seen), we only do one and without any allocs.
    if let Some(count) = counts.get_mut(word) {
        *count += 1;
        return;
    }
    counts.insert(word.to_vec(), 1);
}
