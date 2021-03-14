#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

int main() {
    std::ios::sync_with_stdio(false);

    std::string word;
    std::unordered_map<std::string, int> counts;
    while (std::cin >> word) {
        std::transform(word.begin(), word.end(), word.begin(),
            [](unsigned char c){ return std::tolower(c); });
        counts[word]++;
    }
    if (std::cin.bad()) {
        std::cerr << "error reading stdin\n";
        return 1;
    }

    std::vector<std::pair<std::string, int>> ordered(counts.begin(),
        counts.end());
    std::sort(ordered.begin(), ordered.end(),
        [](auto const& a, auto const& b) { return a.second>b.second; });

    for (auto const& count : ordered) {
        std::cout << count.first << " " << count.second << "\n";
    }
}
