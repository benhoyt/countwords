package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"sort"
)

func main() {
	var n int
	var offset int
	var remainder int
	var err error
	var buf = make([]byte, 64*1024)
	var counts = map[string]*Count{}
	for {
		// Read input in 64KB blocks till EOF.
		n, err = os.Stdin.Read(buf[remainder:])
		if err != nil && err != io.EOF {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		if n == 0 {
			break
		}
		n += remainder
		offset = 0

		// Count words in the buffer.
		for i := 0; i < n; i++ {
			if buf[i] <= ' ' {
				// Found a whitespace char, count last word.
				if i > offset {
					increment(counts, buf[offset:i])
				}
				offset = i+1
			} else if buf[i] >= 'A' && buf[i] <= 'Z' {
				// Convert to ASCII lowercase as we go.
				buf[i] += 'a' - 'A'
			}
		}
		remainder = n-offset
		copy(buf[:remainder], buf[offset:n])
	}

	// Count last word, if any.
	if offset < n {
		increment(counts, buf[offset:n])
	}

	// Convert to slice of Count, sort by count descending, print.
	ordered := make([]*Count, 0, len(counts))
	for _, count := range counts {
		ordered = append(ordered, count)
	}
	sort.Slice(ordered, func(i, j int) bool {
		return ordered[i].Count > ordered[j].Count
	})
	out := bufio.NewWriterSize(os.Stdout, 64*1024)
	for _, count := range ordered {
		fmt.Fprintln(out, count.Word, count.Count)
	}
	out.Flush()
}

func increment(counts map[string]*Count, word []byte) {
	if p, ok := counts[string(word)]; ok {
		// Word already in map, increment existing Count.
		p.Count++
	} else {
		// Word not in map, insert new Count.
		counts[string(word)] = &Count{string(word), 1}
	}
}

type Count struct {
	Word  string
	Count int
}
