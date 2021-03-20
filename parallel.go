package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"runtime"
	"runtime/trace"
	"sort"
	"strings"
)

var (
	procFactor = 1.0

	minChunkSize int = 64 * 1024

	tokenSplit = bufio.ScanWords
)

func main() {
	traceTo := flag.String("trace", "", "enable runtime tracing")
	flag.IntVar(&minChunkSize, "min-chunk-size", minChunkSize, "minimum chunk size for concurrent input splitting")
	flag.Float64Var(&procFactor, "proc-factor", procFactor, "processor over-scheduling factor; set to 0 to disable concurrent processing")
	flag.Parse()

	if *traceTo != "" {
		f, err := os.Create(*traceTo)
		if err == nil {
			defer f.Close()
			err = trace.Start(f)
		}
		if err != nil {
			log.Fatalln("unable to start trace", err)
		}
		defer trace.Stop()
	}

	if err := errMain(); err != nil {
		log.Fatal(err)
	}
}

func errMain() error {
	var (
		in  = os.Stdin
		out = os.Stdout

		numProcs  = runtime.GOMAXPROCS(-1)
		numChunks = int(float64(numProcs) * procFactor)
	)

	// precompute token-aligned chunk boundaries
	var chunks []chunk
	if procFactor > 0 {
		var err error
		chunks, err = splitChunks(numChunks, minChunkSize, in, tokenSplit)

		// try to fixup (things like illegal seek error) by soaking into a temp file and re-chunking
		// TODO would be better if we had a portable form of errors.Is(err, syscall.EILSEQ)
		if err != nil {
			var tmp *os.File
			tmp, err = os.CreateTemp("", "")
			if err == nil {
				defer os.Remove(tmp.Name())
				if _, err = io.Copy(tmp, in); err == nil {
					in.Close()
					in = tmp
					if _, err = in.Seek(0, os.SEEK_SET); err == nil {
						chunks, err = splitChunks(numChunks, minChunkSize, in, tokenSplit)
					}
				}
			}
		}

		if err != nil {
			return err
		}
	}

	// run serially if input not large enough or forced
	if len(chunks) <= 1 {
		counts, err := countTokens(in, tokenSplit)
		if err == nil {
			err = counts.report(out)
		}
		return err
	}

	// error stream from counter goroutines to parent watcher
	errch := make(chan error, len(chunks))

	// result channels form a binary tree, with each counter goroutine merging
	// from up to two downstream children before sending its result upstream
	results := make(map[int]chan result, len(chunks))
	for i := range chunks {
		results[i] = make(chan result, 1)
	}

	for i, c := range chunks {
		go func(
			chunk chunk,

			resto chan<- result,
			errto chan<- error,

			// NOTE this isn't variadic because no dynamic select (reflect hack isn't worth it imo)
			left, right <-chan result,
		) {
			defer close(resto)

			// scan and count tokens within our chunk
			counts, err := countTokens(chunk.sectionReader(in), tokenSplit)
			if err != nil {
				errto <- err
				return
			}

			// wait for and merge results from both children
			for left != nil || right != nil {
				select {

				case other, ok := <-left:
					if !ok {
						left = nil
					} else {
						counts.merge(other)
					}

				case other, ok := <-right:
					if !ok {
						right = nil
					} else {
						counts.merge(other)
					}

				}
			}

			resto <- counts
		}(c, results[i], errch, results[2*i+1], results[2*i+2])
	}
	final := results[0]

	// wait for error or final result
	for final != nil || errch != nil {
		select {
		case err, ok := <-errch:
			if !ok {
				errch = nil
			} else if err != nil {
				return err
			}
		case counts := <-final:
			final = nil
			close(errch)
			if err := counts.report(out); err != nil {
				return err
			}
		}
	}
	return nil
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

func (counts result) merge(other result) {
	for word, count := range other {
		counts[word] += count
	}
}

type chunk struct {
	off int64
	end int64
}

func (c chunk) size() int64 {
	return c.end - c.off
}

func (c chunk) sectionReader(in io.ReaderAt) *io.SectionReader {
	return io.NewSectionReader(in, c.off, c.size())
}

// splitChunks makes a best effort to generate numChunks ranges of at least
// minChunks size within the byte space of a random access reader.
// Chunks will always end at a valid token boundary as defined by split.
// Performs small reads to find token boundaries near chunk edges.
func splitChunks(numChunks, minSize int, in io.ReaderAt, split bufio.SplitFunc) ([]chunk, error) {
	if numChunks < 2 {
		return nil, nil
	}

	size, err := readerSize(in)
	if err != nil {
		return nil, err
	}
	if size == 0 {
		return nil, fmt.Errorf("unable to determine the size of %T ReaderAt", in)
	}

	const (
		minPeek  = 64
		maxAllow = minPeek * 8
	)
	if minSize < maxAllow {
		minSize = maxAllow
	}

	chunkSize := size/int64(numChunks) + maxAllow
	if minSize > 0 && chunkSize < int64(minSize) {
		chunkSize = int64(minSize)
	}

	chunks := make([]chunk, 0, numChunks)

	// NOTE extreme upper sanity bound should never be hit, since we should
	// break because EOF long before 2x chunks
	var off int64 = 0
	var buf []byte
	for sanity := numChunks * 2; sanity > 0; sanity-- {
		// compute chunk end at next token boundary
		end := off + chunkSize
		var (
			peek []byte
			err  error
		)
		for sz := int64(minPeek); peek == nil && err == nil; sz *= 2 {
			if cap(buf) < int(sz) {
				buf = make([]byte, int(sz))
			}
			if end -= sz; end < off {
				end = off
			}
			peek, err = readChunk(buf[:sz], in, end, split)
		}
		if err == io.EOF {
			end = size
		} else if err != nil {
			return nil, err
		} else {
			end += int64(len(peek))
		}

		chunks = append(chunks, chunk{off, end})

		// keep splitting until EOF
		if err == io.EOF {
			break
		}
		off = end
	}

	return chunks, nil
}

// readChunk tries to read a token-aligned chunk of input from a given offset
// within a random access input into buf. If the split function declines to
// tokenize, returns nil and any split or read error. When successful, returns
// a non-empty sub-slice of buf that contains a usable chunk, along with io.EOF
// if the underlying ReadAt returned EOF.
//
// Uses a from of binary search to call split order-of-log(size) times,
// attempting to maximize the returned chunk size.
func readChunk(buf []byte, in io.ReaderAt, off int64, split bufio.SplitFunc) ([]byte, error) {
	n, rerr := in.ReadAt(buf, off)
	atEOF := rerr == io.EOF
	if !atEOF && rerr != nil {
		return nil, rerr
	}
	chunk := buf[:n]

	lo, _, err := split(chunk, atEOF)
	if err != nil {
		return nil, err
	} else if lo == 0 {
		return nil, rerr
	}
	hi := len(chunk)

	for lo < hi {
		mid := lo/2 + hi/2
		adv, _, err := split(chunk[mid:], atEOF)
		if err != nil {
			return nil, err
		}
		if adv > 0 {
			lo = mid + adv
		} else {
			hi = mid
		}
	}
	return chunk[:lo], rerr
}

// readerSize tries to determine the size of a random access reader, supporting
// in-memory implementations (like strings.Reader) and stat-able
// implementations like *os.File.
func readerSize(ra io.ReaderAt) (int64, error) {
	type sizer interface {
		Size() int64
	}
	if sz, ok := ra.(sizer); ok {
		return sz.Size(), nil
	}

	type stater interface {
		Stat() (os.FileInfo, error)
	}
	if st, ok := ra.(stater); ok {
		info, err := st.Stat()
		if err != nil {
			return 0, err
		}
		return info.Size(), nil
	}

	return 0, nil
}
