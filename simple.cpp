#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>
#include <unordered_map>


int main() {
    std::string word;
    std::unordered_map<std::string, int> counts;
    while (std::cin >> word) {
        std::transform(word.begin(), word.end(), word.begin(),
            [](unsigned char c){ return std::tolower(c); });
        ++counts[word];
    }
    if (std::cin.bad()) {
        std::cerr << "error reading stdin\n";
        return 1;
    }

  
    for (auto const elem : counts) {
        std::cout << elem.first << " " << elem.second << "\n";
    }
}
