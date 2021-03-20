package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"sort"
	"strings"
)

var (
	tokenSplit = bufio.ScanWords
)

func main() {
	flag.Parse()

	if err := errMain(); err != nil {
		log.Fatal(err)
	}
}

func errMain() error {
	var (
		in  = os.Stdin
		out = os.Stdout
	)

	counts, err := countTokens(in, tokenSplit)
	if err == nil {
		err = counts.report(out)
	}
	return err
}

type result map[string]int

// countTokens is the core token scanning and counting routine
func countTokens(in io.Reader, split bufio.SplitFunc) (result, error) {
	scanner := bufio.NewScanner(in)
	scanner.Split(split)
	counts := make(result)
	for scanner.Scan() {
		word := strings.ToLower(scanner.Text())
		counts[word]++
	}
	return counts, scanner.Err()
}

// report writes sorted counts to an output writer
func (counts result) report(out io.Writer) error {
	type Count struct {
		Word  string
		Count int
	}

	ordered := make([]Count, 0, len(counts))
	for word, count := range counts {
		ordered = append(ordered, Count{word, count})
	}
	sort.Slice(ordered, func(i, j int) bool {
		return ordered[i].Count > ordered[j].Count
	})

	for _, count := range ordered {
		if _, err := fmt.Fprintf(out, "%v %v\n", count.Word, count.Count); err != nil {
			return err
		}
	}
	return nil
}

