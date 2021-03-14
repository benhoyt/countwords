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
    let mut counts = Trie::new();

    let mut buf = vec![0; 64 * (1<<10)];
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

    let mut ordered: Vec<(Vec<u8>, u64)> = counts.frequencies();
    ordered.sort_by(|&(_, cnt1), &(_, cnt2)| cnt1.cmp(&cnt2).reverse());

    let mut stdout = io::stdout();
    for (word, count) in ordered {
        writeln!(stdout, "{} {}", std::str::from_utf8(&word)?, count)?;
    }
    Ok(())
}

const NODE_SIZE: usize = 256;

/// An ID that doubles as an index into a trie's node table. It is
/// premultiplied, so that given a node ID 'N' and a byte 'b', computing
/// its child only requires an addition: 'trie.nodes[N + b]'. (The
/// typical formulation wouldn't premultiply the ID, and thus, you'd need
/// 'trie.nodes[(N * 256) + b]'.)
type TrieNodeID = std::num::NonZeroUsize;

/// A trie that stores its nodes contiguously in memory.
#[derive(Clone, Debug)]
struct Trie {
    /// A row-major contiguous allocation of trie nodes.
    nodes: Vec<Option<TrieNodeID>>,
    /// A count exists for each node in the trie. For a node ID 'N', one can
    /// get its count via 'counts[N / 256]'.
    counts: Vec<u64>,
}

/// A borrow view of a single trie node. Used for traversing the trie to
/// collect the words and their frequencies.
#[derive(Clone, Debug)]
struct TrieNode<'a> {
    children: &'a [Option<TrieNodeID>],
    count: u64,
}

impl Trie {
    /// Create a new empty trie.
    fn new() -> Trie {
        let mut trie = Trie { nodes: vec![], counts: vec![] };
        // add dummy node with id==0. This is never used. We explicitly add it
        // to avoid needing to subtract 1 on every node lookup. The root starts
        // at id==1.
        trie.nodes.extend(std::iter::repeat(None).take(256));
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

    fn alloc_child(&mut self, current_id: TrieNodeID, b: u8) -> TrieNodeID {
        let child = self.alloc_node();
        self.nodes[current_id.get() + b as usize] = Some(child);
        child
    }

    /// Returns the child node ID of the given current node for the given byte.
    fn child(&self, current_id: TrieNodeID, b: u8) -> Option<TrieNodeID> {
        self.nodes[current_id.get() + b as usize]
    }

    /// Allocates a new node in this trie (with all children empty) and returns
    /// its ID.
    fn alloc_node(&mut self) -> TrieNodeID {
        // This is correct since new() allocates space for a dummy node, so
        // self.counts.len() is always at least 1.
        let id = TrieNodeID::new(self.counts.len() * NODE_SIZE).unwrap();
        self.nodes.extend(std::iter::repeat(None).take(256));
        self.counts.push(0);
        id
    }

    fn increment(&mut self, id: TrieNodeID) {
        self.counts[id.get() / NODE_SIZE] += 1;
    }

    fn is_root(&self, id: TrieNodeID) -> bool {
        id.get() == 1
    }

    fn node<'a>(&'a self, id: TrieNodeID) -> TrieNode<'a> {
        let count = self.counts[id.get() / NODE_SIZE];
        let start = id.get();
        let end = start + NODE_SIZE;
        TrieNode { children: &self.nodes[start..end], count }
    }

    fn frequencies(&self) -> Vec<(Vec<u8>, u64)> {
        struct NodeWithChild {
            id: TrieNodeID,
            byte: usize,
        }

        let byte = self.node(self.root()).next_child(0);
        let mut stack = vec![NodeWithChild { id: self.root(), byte }];
        let mut word = vec![];
        let mut freqs = vec![];

        while let Some(NodeWithChild { id, byte }) = stack.pop() {
            let node = self.node(id);
            if byte >= 256 {
                word.pop();
                continue;
            }
            stack.push(NodeWithChild {
                id, byte: node.next_child(byte + 1),
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
        for i in at_or_after..=(u8::MAX as usize) {
            if self.children[i].is_some() {
                return i;
            }
        }
        256
    }
}
