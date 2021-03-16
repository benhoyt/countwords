// This version is an approximate port of the optimized C program, which rolls
// its own custom hash table. The main benefit here, I think, is that the
// hash can be computed as the bytes are visited in the buffer. When compared
// to using std's hashmap, this saves a pass over the bytes of each word. It
// otherwise uses approximately the same buffering technique as the optimized
// Rust version.
//
// On my system, the performance of this program is just about on par with the
// optimized C variant when the C program is compiled with clang. That same C
// program compiled with gcc is measurably faster by a bit. I didn't dig into
// the codegen to figure out why.

use std::{
    error::Error,
    io::{self, Read, Write},
};

fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn try_main() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut counts = Table::new();
    let mut buf = vec![0; 64 * (1<<10)];
    let mut offset = 0;
    let mut start = None;
    let mut hash = FNV_OFFSET;
    loop {
        let nread = stdin.read(&mut buf[offset..])?;
        if nread == 0 {
            if offset > 0 {
                counts.increment(&buf[..offset+1], hash);
            }
            break;
        }
        let buf = &mut buf[..offset+nread];

        for i in (0..buf.len()).skip(offset) {
            let b = buf[i];
            if b == b' ' || b == b'\n' {
                if let Some(start) = start.take() {
                    counts.increment(&buf[start..i], hash);
                    hash = FNV_OFFSET;
                }
            } else {
                // 0x20 (6th bit) is the only different bit between lowercase and uppercase
                buf[i] |= 0x20;
                if start.is_none() {
                    start = Some(i);
                }
                hash *= FNV_PRIME;
                hash ^= buf[i] as u64;
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

    let mut ordered = counts.into_counts();
    ordered.sort_unstable_by_key(|&(_, count)| count);

    for (word, count) in ordered.into_iter().rev() {
        writeln!(io::stdout(), "{} {}", std::str::from_utf8(&word)?, count)?;
    }
    Ok(())
}

const HASH_LEN: usize = 64 * (1<<10); // mut be >= number of unique words
const FNV_OFFSET: u64 = 14695981039346656037;
const FNV_PRIME: u64 = 1099511628211;

#[derive(Clone, Debug)]
struct Table {
    entries: Vec<TableEntry>,
}

#[derive(Clone, Debug)]
struct TableEntry {
    word: Option<Box<[u8]>>,
    count: u64,
}

impl Table {
    fn new() -> Table {
        Table { entries: vec![TableEntry { word: None, count: 0 }; HASH_LEN] }
    }

    fn into_counts(self) -> Vec<(Box<[u8]>, u64)> {
        let mut counts = Vec::with_capacity(self.entries.len());
        for e in self.entries {
            let word = match e.word {
                None => continue,
                Some(word) => word,
            };
            counts.push((word, e.count));
        }
        counts
    }

    fn increment(&mut self, word: &[u8], hash: u64) {
        let mut index = (hash % (HASH_LEN as u64)) as usize;
        // if all entries are full, then this loop will never terminate. Alas,
        // we copy this limitation from the C implementation. Basically,
        // HASH_LEN must be greater than the total number of unique words.
        loop {
            let mut entry = &mut self.entries[index];
            if let Some(eword) = entry.word.as_deref() {
                if eword == word {
                    entry.count += 1;
                    return;
                }
                index = (index + 1) % HASH_LEN;
            } else {
                entry.word = Some(word.to_vec().into_boxed_slice());
                entry.count = 1;
                return;
            }
        }
    }
}
