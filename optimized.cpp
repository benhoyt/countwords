#include <algorithm>
#include <cinttypes>
#include <cassert>
#include <iostream>
#include <iterator>
#include <string_view>

template <std::size_t hash_size, std::size_t words_buf_size>
class table {
  enum : std::uint64_t { hash_init = 0xcbf29ce484222325ULL };
  enum : std::uint64_t { hash_next = 0x100000001b3ULL };
  struct range { int place; std::uint16_t sz; }; // { offset into words, size }
  struct cell { int count = 0; range word; };    // a hash entry

  std::uint64_t hash = hash_init;
  char* tail = words; char* back = words;
  std::size_t entries = 0;
  cell buckets[hash_size];
  char words[words_buf_size];

  auto word_at(range w) { return std::string_view{words + w.place, w.sz}; }
public:
  void push(char c) {
    assert(std::size_t(back - words) < sizeof(words));
    *back++ = c, hash = hash * hash_next ^ c;
  }
  void count_one() {
    if (back != tail) {
      std::string_view  key{tail, std::size_t(back-tail)};
      auto match = [key,this](range word) {
        return word.sz == key.size() && word_at(word) == key; };
      auto* bucket = &buckets[hash % hash_size]; // op& if hash_size is 2^n
      for (; bucket->count && !match(bucket->word);
        bucket = (bucket == buckets) ? buckets+hash_size-1 : bucket-1) {}
      if (! bucket->count) {
        assert(++entries < hash_size/2);
        range word = {int(tail - words), std::uint16_t(back - tail)};
        *bucket = {1, word }, tail = back;  // keep new word
      } else ++bucket->count, back = tail;  // discard word
      hash = hash_init;
    }
  }
  void dump(std::ostream& out) {
    auto end = std::partition(buckets, buckets + hash_size, [](auto& b){
      return b.count != 0; });
    std::sort(buckets, end, [](cell& a, cell& b) {
      return a.count > b.count; });
    std::for_each(buckets, end, [&](cell& b) {
      out << word_at(b.word) << ' ' << b.count << '\n'; });
  }
};
table<(1<<16), (1<<19)> counts;

int main() {
  std::ios::sync_with_stdio(false);
  auto to_lower = [](unsigned char c) { return c | (-(c-'A' < 26) & '\x20'); };
  for (std::istreambuf_iterator<char> it(std::cin), end; it != end; ++it) {
    if (*it <= ' ') {
      counts.count_one();
    } else counts.push(to_lower(*it));
  }
  counts.count_one(); // in case file ends without whitespace
  counts.dump(std::cout);
}
