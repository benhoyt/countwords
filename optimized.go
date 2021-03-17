package main

import (
	"fmt"
	"io"
	"os"
	"sort"
)

func main() {
	wc := NewWordCounter()
	io.Copy(wc, os.Stdin)

	for _, word := range wc.MostCommon() {
		fmt.Println(word.Word, word.Count)
	}
}

type WordCount struct {
	Word  string
	Count int
}

type WordCounter struct {
	buf   []byte
	words map[string]*int
}

func NewWordCounter() *WordCounter {
	return &WordCounter{
		words: make(map[string]*int),
	}
}

func (wc *WordCounter) addWord() {
	if len(wc.buf) == 0 {
		return
	}

	// Clean buffer after counting a word
	defer func() { wc.buf = wc.buf[:0] }()

	if n, ok := wc.words[string(wc.buf)]; ok {
		*n++
		return
	}

	n := 1
	wc.words[string(wc.buf)] = &n
}

func (wc *WordCounter) MostCommon() []WordCount {
	// Flush any data in the buffer
	wc.addWord()

	counts := make([]WordCount, 0, len(wc.words))
	for word, count := range wc.words {
		counts = append(counts, WordCount{word, *count})
	}

	sort.Slice(counts, func(i, j int) bool {
		return counts[i].Count > counts[j].Count
	})

	return counts
}

func (wc *WordCounter) Write(p []byte) (int, error) {
	for _, b := range p {
		if b == ' ' || b == '\n' {
			wc.addWord()
			continue
		}

		// To lower case
		if b >= 'A' && b <= 'Z' {
			b += 'a' - 'A'
		}

		wc.buf = append(wc.buf, b)
	}

	return len(p), nil
}
