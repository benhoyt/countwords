// This version is similar to optimized except it uses arena to store the
// keys rather than having separate allocation for each new key.
//
// It would have similar results if one uses bumpalo or typed-arena, we
// just didn't use any libraries here, or maybe this one could be slightly
// faster because it uses Cell instead of RefCell.

use std::cell::Cell;
use std::error::Error;
use std::io::{self, BufWriter, Read, Write};

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
    let keys = Cell::new(Vec::with_capacity(256 * 1024)); // more than enough
    let mut counts: HashMap<&[u8], u64> = HashMap::default();
    let mut buf = vec![0; 64 * (1 << 10)];
    let mut offset = 0;
    let mut start = None;
    loop {
        let nread = stdin.read(&mut buf[offset..])?;
        if nread == 0 {
            if offset > 0 {
                increment(&keys, &mut counts, &buf[..offset + 1]);
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
                    increment(&keys, &mut counts, &buf[start..i]);
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

    let stdout = io::stdout();
    let mut stdout = BufWriter::new(stdout.lock());
    for (word, count) in ordered.into_iter().rev() {
        writeln!(stdout, "{} {}", std::str::from_utf8(&word)?, count)?;
    }
    Ok(())
}

fn increment<'a>(keys_outer: &'a Cell<Vec<u8>>, counts: &mut HashMap<&'a [u8], u64>, word: &[u8]) {
    // using 'counts.entry' would be more idiomatic here, but doing so requires
    // allocating a new Vec<u8> because of its API. Instead, we do two hash
    // lookups, but in the exceptionally common case (we see a word we've
    // already seen), we only do one and without any allocs.
    if let Some(count) = counts.get_mut(word) {
        *count += 1;
        return;
    }
    // store keys in another buffer to avoid unnecessary allocation for each key
    let mut keys = keys_outer.take();
    let start = keys.len();
    // make sure if never gets larger than the keys buffer, if it reallocate the memory,
    // the key references may be pointing to the wrong address
    assert!(keys.len() + word.len() <= keys.capacity());
    keys.extend_from_slice(word);
    // Safety: extend the lifetime of keys since are reusing the same buffer like an arena
    let word: &'a [u8] = unsafe { std::mem::transmute(&keys[start..start + word.len()]) };
    counts.insert(word, 1);
    keys_outer.set(keys); // store it back
}
