#include <algorithm>
#include <cctype>
#include <iostream>
#include <iterator>
#include <string>
#include <unordered_map>
#include <vector>

int main() {
  std::unordered_map<std::string, int>  counts;
  std::string word; word.reserve(1024);
  for (std::istreambuf_iterator<char> in(std::cin), end; in != end; ++in) {
    unsigned char c = *in;
    if (std::isspace(c)) {
      if (!word.empty()) {
        ++counts[word];
        word.clear();
      }
    } else word.push_back(std::tolower(c));
  }
  if (!word.empty()) {  // in case file ends with no EOL
    ++counts[word];
  }
  std::vector<std::pair<const std::string,int> const*>  vec;
  vec.reserve(1<<15);
  for (auto const& word : counts) {
    vec.push_back(&word);
  }
  std::sort(vec.begin(), vec.end(), [](auto const* a, auto const* b) {
    return a->second > b->second;
  });
  for (auto const* word : vec) {
    std::cout << word->first << ' ' << word->second << '\n';
  }
}
