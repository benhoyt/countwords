// Print frequencies of unique words in text file, most frequent first

package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	counts := make(map[string]int)
	for scanner.Scan() {
		line := strings.ToLower(scanner.Text())
		for _, word := range strings.Fields(line) {
			counts[word]++
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
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
