package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"runtime/pprof"
	"sort"
)

func main() {
	f, err := os.Create("cpuprofile")
	if err != nil {
		fmt.Fprintf(os.Stderr, "could not create CPU profile: %v\n", err)
		os.Exit(1)
	}
	if err := pprof.StartCPUProfile(f); err != nil {
		fmt.Fprintf(os.Stderr, "could not start CPU profile: %v\n", err)
		os.Exit(1)
	}
	defer pprof.StopCPUProfile()

	input, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	counts := make(map[string]int)

	start := -1
	for i, c := range input {
		if c >= 'A' && c <= 'Z' {
			c = c + ('a' - 'A')
			input[i] = c
		}
		if start >= 0 {
			if c == ' ' || c == '\n' {
				counts[string(input[start:i])]++
				start = -1
			}
		} else {
			if c != ' ' && c != '\n' {
				start = i
			}
		}
	}

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
