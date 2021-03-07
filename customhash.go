package main

import (
	// "bufio"
	"bytes"
	"fmt"
	// "io/ioutil"
	"io"
	"os"
	// "runtime/pprof"
	"sort"
)

func main() {
	// f, err := os.Create("cpuprofile")
	// if err != nil {
	// 	fmt.Fprintf(os.Stderr, "could not create CPU profile: %v\n", err)
	// 	os.Exit(1)
	// }
	// if err := pprof.StartCPUProfile(f); err != nil {
	// 	fmt.Fprintf(os.Stderr, "could not start CPU profile: %v\n", err)
	// 	os.Exit(1)
	// }
	// defer pprof.StopCPUProfile()

	buf := make([]byte, 64*1024)
	offset := 0
	var counts Counter
	// counts := make(map[string]int)
	for {
		n, err := os.Stdin.Read(buf[offset:])
		if err != nil && err != io.EOF {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		if n == 0 {
			break
		}
		chunk := buf[:offset+n]
		lastLF := bytes.LastIndexByte(chunk, '\n')
		process := chunk
		if lastLF != -1 {
			process = chunk[:lastLF]
		}

		start := -1
		for i, c := range process {
			if c >= 'A' && c <= 'Z' {
				c = c + ('a' - 'A')
				process[i] = c
			}
			if start >= 0 {
				if c == ' ' || c == '\n' {
					counts.Count(process[start:i], 1)
					// counts[string(process[start:i])]++
					start = -1
				}
			} else {
				if c != ' ' && c != '\n' {
					start = i
				}
			}
		}
		if start >= 0 && start < len(process) {
			counts.Count(process[start:], 1)
			// counts[string(process[start:])]++
		}

		// for _, word := range bytes.Fields(toLower(process)) {
		// 	counts[string(word)]++
		// }

		if lastLF == -1 {
			offset = 0
		} else {
			remaining := chunk[lastLF+1:]
			copy(buf, remaining)
			offset = len(remaining)
		}
	}

	ordered := counts.Items()
	sort.Slice(ordered, func(i, j int) bool {
		return ordered[i].Count > ordered[j].Count
	})

	for _, count := range ordered {
		fmt.Printf("%s %d\n", string(count.Key), count.Count)
	}

	// var ordered []Count
	// for word, count := range counts {
	// 	ordered = append(ordered, Count{word, count})
	// }
	// sort.Slice(ordered, func(i, j int) bool {
	// 	return ordered[i].count > ordered[j].count
	// })

	// for _, count := range ordered {
	// 	fmt.Printf("%s %d\n", count.word, count.count)
	// }
}

type Counter struct {
	items []CounterItem
	size  int
}

type CounterItem struct {
	Key   []byte
	Count int
}

const (
	offset64 = 14695981039346656037
	prime64  = 1099511628211
)

func (c *Counter) Count(word []byte, n int) {
	// Like hash/fnv New64, Write, Sum64 -- but inlined
	hash := uint64(offset64)
	for _, c := range word {
		hash *= prime64
		hash ^= uint64(c)
	}

	h := int(hash & uint64(len(c.items)-1))

	if c.size >= len(c.items)/2 {
		// Current items more than half full, double items length
		newLen := len(c.items) * 2
		if newLen == 0 {
			newLen = 1024
		}
		newC := Counter{items: make([]CounterItem, newLen)}
		for _, item := range c.items {
			if item.Key != nil {
				newC.Count(item.Key, item.Count)
			}
		}
		c.items = newC.items
		h = int(hash & uint64(len(c.items)-1))
	}

	for {
		if c.items[h].Key == nil {
			// Found empty slot, insert
			wordCopy := make([]byte, len(word))
			copy(wordCopy, word)
			c.items[h] = CounterItem{wordCopy, n}
			c.size++
			return
		}
		if bytes.Equal(c.items[h].Key, word) {
			// Found matching slot, increment
			c.items[h].Count += n
			return
		}
		// Slot already holds another key, linear probe
		h++
		if h >= len(c.items) {
			h = 0
		}
	}
}

func (c *Counter) Items() []CounterItem {
	var items []CounterItem
	for _, item := range c.items {
		if item.Key != nil {
			items = append(items, item)
		}
	}
	return items
}

type Count struct {
	word  string
	count int
}

// A split function for a Scanner that returns each space-separated word of
// text, with surrounding spaces deleted. It will never return an empty
// string. ASCII-only.
func scanWordsASCII(data []byte, atEOF bool) (advance int, token []byte, err error) {
	// Skip leading spaces.
	start := 0
	for ; start < len(data); start++ {
		if !isSpace(data[start]) {
			break
		}
	}
	// Scan until space, marking end of word.
	for i := start; i < len(data); i++ {
		if isSpace(data[i]) {
			return i + 1, toLower(data[start:i]), nil
		}
	}
	// If we're at EOF, we have a final, non-empty, non-terminated word. Return it.
	if atEOF && len(data) > start {
		return len(data), toLower(data[start:]), nil
	}
	// Request more data.
	return start, nil, nil
}

func isSpace(c byte) bool {
	switch c {
	case ' ', '\t', '\n', '\r':
		return true
	default:
		return false
	}
}

func toLower(s []byte) []byte {
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c >= 'A' && c <= 'Z' {
			s[i] = c + ('a' - 'A')
		}
	}
	return s
}
