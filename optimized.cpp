#include <algorithm>
#include <cctype>
#include <string>
#include <unordered_map>
#include <vector>
#include <iostream>
#include <cassert>

#ifdef USE_ABSEIL
#include <absl/container/flat_hash_map.h>
typedef absl::flat_hash_map<std::string, int> WordMap;
#else
typedef std::unordered_map<std::string, int> WordMap;
#endif

size_t split_buffer(WordMap &counts, char *buf, size_t data_size) {
    size_t i=0;
    std::string word;
    while(true) {
        while(i<data_size && (buf[i] == ' ' || buf[i] == '\n')) {
            ++i;
        }
        if(i == data_size) {
            // Nothing but spaces.
            break;
        }
        size_t word_start = i;
        while(i<data_size && (buf[i] != ' ' && buf[i] != '\n')) {
            if(buf[i] >= 'A' && buf[i] <= 'Z') {
                buf[i] += 'a'-'A';
            }
            ++i;
        }
        if(i == data_size) {
            // Reached the end of the buffer. Do not use this
            // as the word may continue in the next data block;
            return word_start;
        }
        size_t word_end = i;
        word.clear();
        word.append(buf + word_start, word_end - word_start);
        ++counts[word];
    }
    return i;
}

int main(int argc, char **argv) {
    WordMap counts;
    counts.reserve(10000);

    const int bufsize = 8*1024;
    char buf[bufsize];
    int i=0;
    FILE *f;
    if(argc == 2) {
        f = fopen(argv[1], "r");
    } else {
        f = stdin;
    }
    while (true) {
        size_t num_read = fread(buf+i, 1, bufsize-i, f);
        if (num_read == 0) {
            break;
        }
        size_t data_size = i + num_read;
        auto bytes_consumed = split_buffer(counts, buf, data_size);
        std::copy(buf+bytes_consumed, buf+data_size, buf);
        i = data_size - bytes_consumed;
    }

    std::vector<std::pair<std::string, int>> ordered(counts.begin(),
        counts.end());
    std::sort(ordered.begin(), ordered.end(),
        [](auto const& a, auto const& b) { return a.second>b.second; });

    for (auto const& count : ordered) {
        std::cout << count.first << " " << count.second << "\n";
    }
}
