
#include <stdio.h>

#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <algorithm>

using namespace std;

bool sort_second(const pair<string,int> &a, const pair<string,int> &b) { 
    return a.second > b.second;
}

int main() {
    string word;
    unordered_map<string, int> counts;
    char buf[1000];
    while (scanf("%s", buf) != EOF) {
        string word = string(buf);
        transform(word.begin(), word.end(), word.begin(), ::tolower);
        counts[word]++;
    }

    vector<pair<string, int> > vect;
    vect.reserve(counts.size());
    for (unordered_map<string, int>::iterator it = counts.begin(); it != counts.end(); it++) {
        vect.push_back(make_pair(it->first, it->second));
    }
    sort(vect.begin(), vect.end(), sort_second); 


    for (vector<pair<string, int> >::iterator it = vect.begin(); it != vect.end(); it++) {
        cout << it->first << " " << it->second << "\n";
    }
    return 0;
}
