#include <algorithm>
#include <cinttypes>
#include <iostream>
#include <iterator>
#include <string_view>
#include <vector>

struct table {
  static const inline uint64_t
    size = 1<<16, hinit = 0xcbf29ce484222325ULL, hnext = 0x100000001b3ULL;
  struct range { int b; std::uint16_t sz; }; // { offset into words, len }
  struct cell { int count = 0; range word; };  // a hash entry

  std::uint64_t hash = hinit;
  int words_end = 0;
  std::vector<char> words;
  cell buckets[size];

  explicit table() { words.reserve(1<<19); }
  void push(char c) { words.push_back(c); hash = hash * hnext ^ c; }
  std::string_view word_at(range w) { return {&words[w.b], w.sz}; }
  bool match(range a, range b) {
    return a.sz == b.sz && word_at(a) == word_at(b);
  }
  void count_one() {
    if (int(words.size()) != words_end) {
      range word = {words_end, std::uint16_t(words.size() - words_end)};
      auto* bucket = &buckets[hash & (size-1)];
      while (bucket->count && !match(bucket->word, word)) {
        bucket = (bucket == buckets) ? buckets + size - 1 : bucket - 1;
      }
      if (! bucket->count) {
        *bucket = {1, word}, words_end += word.sz;
      } else ++bucket->count, words.resize(words_end);
      hash = hinit;
    }
  }
  void sort() {
    std::sort(buckets, buckets + size, [](cell const& a, cell const& b) {
      return a.count > b.count; });
  }
  void dump(std::ostream& os) {
    for (auto& b: buckets) {
      if (b.count) {
        os << word_at(b.word) << ' ' << b.count << '\n';
      } else break;
    }
  }
};

int main() {
  std::ios::sync_with_stdio(false);
  auto to_lower = [](unsigned char c) { return (c-'A' < 26) ? c|'\x20' : c; };
  table counts;
  for (std::istreambuf_iterator<char> it(std::cin), end; it != end; ++it) {
    char c = *it;
    if (c <= ' ') {
      counts.count_one();
    } else {
      counts.push(to_lower(c));
    }
  }
  counts.sort();
  counts.dump(std::cout);
}
