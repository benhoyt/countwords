#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

int main() {
    string word;
    unordered_map<string, int> counts;
    while (cin >> word) {
        transform(word.begin(), word.end(), word.begin(),
            [](unsigned char c){ return tolower(c); });
        counts[word]++;
    }

    vector<pair<string, int>> ordered(counts.begin(), counts.end());
    sort(ordered.begin(), ordered.end(), [](auto &a, auto &b) {
        return a.second > b.second;
    });

    for (auto count : ordered) {
        cout << count.first << " " << count.second << "\n";
    }
    return 0;
}
