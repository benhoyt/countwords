// This version was an experiment to write a faster version of the word
// counting program by using a trie. Ultimately, while it's pretty fast, it's
// not as fast the optimized Go, Rust or C versions using a standard hashmap.
// My hypothesis was that the trie avoids the need to hash each word, and thus,
// let's us inspect each byte only once. But this comes at the expense of an
// additional memory operation for each byte (to traverse the trie). I made
// that as cheap as I could, but alas, profiling seems to suggest it is still
// too expensive.
//
// Of course, the trie in this program is quite reckless in terms of memory
// usage. But, it only scales with the number of unique words, and memory usage
// doesn't appear to be a metric in this particular exercise.
//
// Also, since the trie is a finite state machine itself, it somewhat
// simplifies the buffer handling in the code below. There's no need to keep
// track of incomplete words/lines. You just advance the state machine. Alas,
// this doesn't really help with performance, since the tracking is only done
// once per buffer read.

use std::{
    convert::TryFrom,
    error::Error,
    io::{self, BufWriter, Read, Write},
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
    let mut counts = Trie::new();

    let mut buf = vec![0; 64 * (1 << 10)];
    let mut node_id = counts.root();
    loop {
        let nread = stdin.read(&mut buf)?;
        if nread == 0 {
            break;
        }
        let buf = &buf[..nread];

        for i in 0..buf.len() {
            let mut b = buf[i];
            if b == b' ' || b == b'\n' || b == b'\r' {
                if !counts.is_root(node_id) {
                    counts.increment(node_id);
                    node_id = counts.root();
                }
            } else {
                if b'A' <= b && b <= b'Z' {
                    b += b'a' - b'A';
                }
                node_id = counts.add_child(node_id, b);
            }
        }
    }
    if !counts.is_root(node_id) {
        counts.increment(node_id);
    }

    let mut ordered = counts.frequencies();
    ordered.sort_unstable_by_key(|&(_, count)| count);

    let stdout = io::stdout();
    let mut stdout = BufWriter::new(stdout.lock());
    for (word, count) in ordered.into_iter().rev() {
        writeln!(stdout, "{} {}", std::str::from_utf8(&word)?, count)?;
    }
    Ok(())
}

/// Since the challenge only requires dealing with ASCII, we can keep out
/// NODE_SIZE to the ASCII range of bytes. This halves the size of the trie
/// node table.
///
/// Technically, there are only 47 unique bytes in the input, so this could in
/// theory be halved again. But you'd need some way to (very quickly) map the
/// bytes into the smaller range, since the maximum byte value is 122. At this
/// point, I think it probably starts violating the spirit of the challenge
/// anyway. Coupling the program too tightly to a specific input is bad juju.
///
/// (At the very least, this should be a power-of-2. Otherwise, shifts get
/// turned into more costly DIV instructions.)
const NODE_SIZE: usize = 128;

/// An ID that doubles as an index into a trie's node table. It is
/// premultiplied, so that given a node ID 'N' and a byte 'b', computing
/// its child only requires an addition: 'trie.nodes[N + b]'. (The
/// typical formulation wouldn't premultiply the ID, and thus, you'd need
/// 'trie.nodes[(N * NODE_SIZE) + b]'.)
///
/// We use a u32 here instead of usize to cut the size of the trie node table
/// in half.
///
/// Also, note that Option<NonZeroU32> is the same size in memory as a u32!
///
/// We could use a NonZeroU16 here to halve the table size again, but then we
/// wouldn't be able to pre-multiply the node IDs. (Premultiplying them causes
/// the IDs to be bigger than a u16::MAX.) In experiments, it looks like a
/// wash.
type TrieNodeID = std::num::NonZeroU32;

/// A trie that stores its nodes contiguously in memory.
#[derive(Clone, Debug)]
struct Trie {
    /// A row-major contiguous allocation of trie nodes.
    ///
    /// The performance of this program seems highly sensitive to how big this
    /// table is in memory. The smaller it is, the higher our cache hit rate
    /// and the faster our program. See comments above for how NODE_SIZE and
    /// the representation of TrieNodeID impact this.
    ///
    /// It's not quite clear how to shrink the table more. It's possible that
    /// a strategy based on the frequency distribution of bytes that we see
    /// (e.g., b'a'-b'z' are overwhelmingly the most common). That is, we might
    /// have a separate table for common bytes and another larger one to handle
    /// the rest. If most accesses are in the smaller table, then we likely
    /// will increase our cache hit rate. The problem with this strategy is
    /// tying everything together without introducing more overhead. Alas, I
    /// ran out of time.
    nodes: Vec<Option<TrieNodeID>>,
    /// A count exists for each node in the trie. For a node ID 'N', one can
    /// get its count via 'counts[N / NODE_SIZE]'.
    counts: Vec<u32>,
}

/// A borrow view of a single trie node. Used for traversing the trie to
/// collect the words and their frequencies.
#[derive(Clone, Debug)]
struct TrieNode<'a> {
    children: &'a [Option<TrieNodeID>],
    count: u32,
}

impl Trie {
    /// Create a new empty trie.
    fn new() -> Trie {
        let mut trie = Trie {
            nodes: vec![],
            counts: vec![],
        };
        // add dummy node with id==0. This is never used. We explicitly add it
        // to avoid needing to subtract 1 on every node lookup. The root starts
        // at id==1.
        trie.nodes.extend(std::iter::repeat(None).take(NODE_SIZE));
        trie.counts.push(0);
        trie.alloc_node(); // root node
        trie
    }

    /// Return the ID of the root node of this trie.
    fn root(&self) -> TrieNodeID {
        TrieNodeID::new(1).unwrap()
    }

    /// Adds a transition from current_id for the given byte to a child node
    /// if one doesn't exist. Either way, the corresponding child node ID is
    /// returned.
    fn add_child(&mut self, current_id: TrieNodeID, b: u8) -> TrieNodeID {
        let child = self.child(current_id, b);
        if child.is_some() {
            return child.unwrap();
        }
        self.alloc_child(current_id, b)
    }

    /// Allocate a new child for the given args and return its ID.
    fn alloc_child(&mut self, current_id: TrieNodeID, b: u8) -> TrieNodeID {
        let child = self.alloc_node();
        self.nodes[current_id.get() as usize + b as usize] = Some(child);
        child
    }

    /// Returns the child node ID of the given current node for the given byte.
    /// If there is no child for the given args, then None is returned.
    fn child(&self, current_id: TrieNodeID, b: u8) -> Option<TrieNodeID> {
        self.nodes[current_id.get() as usize + b as usize]
    }

    /// Allocates a new node in this trie (with all children empty) and returns
    /// its ID.
    fn alloc_node(&mut self) -> TrieNodeID {
        // This is correct since we assume that there are no more than
        // 2^32/NODE_SIZE trie nodes. In practice, for this challenge, it
        // works.
        let id = self.counts.len().checked_mul(NODE_SIZE).unwrap();
        // This is correct since new() allocates space for a dummy node, so
        // self.counts.len() is always at least 1 and thus id is always greater
        // than 0.
        let id = TrieNodeID::new(u32::try_from(id).unwrap()).unwrap();
        self.nodes.extend(std::iter::repeat(None).take(NODE_SIZE));
        self.counts.push(0);
        id
    }

    fn increment(&mut self, id: TrieNodeID) {
        self.counts[id.get() as usize / NODE_SIZE] += 1;
    }

    fn is_root(&self, id: TrieNodeID) -> bool {
        id.get() == 1
    }

    fn node<'a>(&'a self, id: TrieNodeID) -> TrieNode<'a> {
        let count = self.counts[id.get() as usize / NODE_SIZE];
        let start = id.get() as usize;
        let end = start + NODE_SIZE;
        TrieNode {
            children: &self.nodes[start..end],
            count,
        }
    }

    fn frequencies(&self) -> Vec<(Vec<u8>, u32)> {
        struct NodeWithChild {
            id: TrieNodeID,
            byte: usize,
        }

        let byte = self.node(self.root()).next_child(0);
        let mut stack = vec![NodeWithChild {
            id: self.root(),
            byte,
        }];
        let mut word = vec![];
        let mut freqs = vec![];

        while let Some(NodeWithChild { id, byte }) = stack.pop() {
            let node = self.node(id);
            if byte >= NODE_SIZE {
                word.pop();
                continue;
            }
            stack.push(NodeWithChild {
                id,
                byte: node.next_child(byte + 1),
            });

            // This unwrap is correct because we only ever push non-empty
            // children onto our stack.
            let child_id = node.children[byte].unwrap();
            let child = self.node(child_id);
            word.push(byte as u8);
            if child.count > 0 {
                freqs.push((word.clone(), child.count));
            }
            stack.push(NodeWithChild {
                id: child_id,
                byte: child.next_child(0),
            });
        }
        freqs
    }
}

impl<'a> TrieNode<'a> {
    fn next_child(&self, at_or_after: usize) -> usize {
        for i in at_or_after..NODE_SIZE {
            if self.children[i].is_some() {
                return i;
            }
        }
        NODE_SIZE
    }
}
