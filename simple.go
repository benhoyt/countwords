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
	scanner.Split(bufio.ScanWords)
	counts := make(map[string]int)
	for scanner.Scan() {
		word := strings.ToLower(scanner.Text())
		counts[word]++
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	var ordered []Count
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
