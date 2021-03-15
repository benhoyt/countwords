/+ To compile: ldc2 -O3 -of simple-d simple.d
            or gdc -O3 -o simple-d simple.d
            or dmd -release -of=simple-d simple.d
   GDC is part of GCC so it should be on all modern Linux distributions.
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
