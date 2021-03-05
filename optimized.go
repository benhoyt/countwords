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

	const bufSize = 64 * 1024
	buf := make([]byte, bufSize)
	offset := 0
	counts := make(map[string]int)
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
					counts[string(process[start:i])]++
					start = -1
				}
			} else {
				if c != ' ' && c != '\n' {
					start = i
				}
			}
		}
		if start >= 0 && start < len(process) {
			counts[string(process[start:])]++
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

	// scanner := bufio.NewScanner(os.Stdin)
	// scanner.Split(scanWordsASCII)
	// counts := make(map[string]int)
	// for scanner.Scan() {
	// 	counts[scanner.Text()]++
	// }
	// if err := scanner.Err(); err != nil {
	// 	fmt.Fprintln(os.Stderr, err)
	// 	os.Exit(1)
	// }

	// READ ALL test
	// input, err := ioutil.ReadAll(os.Stdin)
	// if err != nil {
	// 	fmt.Fprintln(os.Stderr, err)
	// 	os.Exit(1)
	// }
	// counts := make(map[string]int)
	// start := -1
	// for i, c := range input {
	// 	if c >= 'A' && c <= 'Z' {
	// 		c = c + ('a' - 'A')
	// 		input[i] = c
	// 	}
	// 	if start >= 0 {
	// 		if c == ' ' || c == '\n' {
	// 			counts[string(input[start:i])]++
	// 			start = -1
	// 		}
	// 	} else {
	// 		if c != ' ' && c != '\n' {
	// 			start = i
	// 		}
	// 	}
	// }

	ordered := make([]Count, 0, len(counts))
	for word, count := range counts {
		ordered = append(ordered, Count{word, count})
	}
	sort.Slice(ordered, func(i, j int) bool {
		return ordered[i].count > ordered[j].count
	})

	for _, count := range ordered {
		fmt.Printf("%s %d\n", count.word, count.count)
	}
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
