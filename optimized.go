package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"sort"
)

func main() {
	offset := 0
	buf := make([]byte, 64*1024)
	counts := make(map[string]*int)
	for {
		// Read input in 64KB blocks till EOF.
		n, err := os.Stdin.Read(buf[offset:])
		if err != nil && err != io.EOF {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		if n == 0 {
			break
		}
		// Offset remaining from last time plus number of bytes read.
		chunk := buf[:offset+n]

		// Find last end-of-line character in block read.
		lastLF := bytes.LastIndexByte(chunk, '\n')
		toProcess := chunk
		if lastLF != -1 {
			toProcess = chunk[:lastLF]
		}

		// Loop through toProcess slice and count words.
		start := -1 // start -1 means in whitespace run
		for i, c := range toProcess {
			// Convert to ASCII lowercase in place as we go.
			if c >= 'A' && c <= 'Z' {
				c = c + ('a' - 'A')
				toProcess[i] = c
			}
			if start >= 0 {
				// In a word, look for end of word (whitespace).
				if c <= ' ' {
					// Count this word!
					increment(counts, toProcess[start:i])
					start = -1
				}
			} else {
				// In whitespace, look for start of word (non-space).
				if c > ' ' {
					start = i
				}
			}
		}
		// Count last word, if any.
		if start >= 0 && start < len(toProcess) {
			increment(counts, toProcess[start:])
		}

		// Copy remaining bytes (incomplete line) to start of buffer.
		if lastLF != -1 {
			remaining := chunk[lastLF+1:]
			copy(buf, remaining)
			offset = len(remaining)
		} else {
			offset = 0
		}
	}

	var ordered []Count
	for word, count := range counts {
		ordered = append(ordered, Count{word, *count})
	}
	sort.Slice(ordered, func(i, j int) bool {
		return ordered[i].Count > ordered[j].Count
	})

	for _, count := range ordered {
		fmt.Println(string(count.Word), count.Count)
	}
}

func increment(counts map[string]*int, word []byte) {
	if p, ok := counts[string(word)]; ok {
		// Word already in map, increment existing int via pointer.
		*p++
		return
	}
	// Word not in map, insert new int.
	n := 1
	counts[string(word)] = &n
}

type Count struct {
	Word  string
	Count int
}
