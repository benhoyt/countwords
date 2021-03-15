/+ To compile: ldc2 -O3 -of optimized-d optimized.d
            or gdc -O3 -o optimized-d optimized.d
            or dmd -release -of=optimized-d optimized.d
+/

import std.stdio;
import std.uni : isWhite;
import std.algorithm.iteration;
import std.algorithm.sorting;
import std.string;
import std.range;
import std.array;
import std.conv;

void main() {
    int[string] arr;
    foreach (line; stdin.byLine()) {
        foreach(word; line.splitter!(x => x.isWhite).filter!(x => !x.empty)) {
            auto k = to!string(word.toLower);
            if (k !in arr) {
                arr[k] = 1;
            } else {
                arr[k]++;
            }
        }
    }
	printSorted(arr);   
}

void printSorted(int[string] arr) {
    auto keys = arr.keys.array;
    foreach (key; keys.sort!((x,y) => arr[x] > arr[y])) {
        writefln!"%s, %s"(key, arr[key]);
    }
}
