#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#ifdef USE_ABSEIL
#include <absl/container/flat_hash_map.h>
typedef absl::flat_hash_map<std::string, int> word_map;
#else
typedef std::unordered_map<std::string, int> word_map;
#endif

constexpr std::size_t max_buffer_size = 1 << 16;

bool is_delimiter(char c) { return c == ' ' || c == '\n' || c == '\r'; }

char simple_ascii_lower(char c) {
  if (c < 'A' || c > 'Z') {
    return c;
  }
  return c + ('a' - 'A');
}

void add_to_counter(word_map &counter, std::string &w) {
  if (!w.empty()) {
    ++counter[w];
    w.clear();
  }
}

int main(int argc, char **argv) {
  std::ios_base::sync_with_stdio(false);

  std::string word;
  std::array<char, max_buffer_size> buffer;
  ssize_t read_size = 0;

  word_map counts;
  counts.reserve(
      25000); // an average english speaker has 25K words of vocabulary

  while ((read_size = std::cin.readsome(buffer.data(), buffer.size())) > 0) {
    std::string_view sbuffer(buffer.data(), read_size);
    for (const auto c : sbuffer) {
      if (!is_delimiter(c)) {
        word.push_back(simple_ascii_lower(c));
      } else {
        add_to_counter(counts, word);
      }
    }
  }
  if (std::cin.bad()) {
    std::cerr << "error reading stdin\n";
    return 1;
  } else {
    add_to_counter(counts, word);
  }

  std::vector<std::pair<std::string_view, int>> ordered(counts.begin(),
                                                        counts.end());
  std::sort(ordered.begin(), ordered.end(),
            [](auto const &a, auto const &b) { return a.second > b.second; });

  for (auto const &count : ordered) {
    std::cout << count.first << ' ' << count.second << '\n';
  }
}
