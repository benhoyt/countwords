#!/usr/bin/env bash

set -e

cat kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt >kjvbible_x10.txt

echo Python simple
python3 simple.py <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Python optimized
python3 optimized.py <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Go simple
go run simple.go <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Go optimized
go run optimized.go <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo C++ simple
g++ -O2 simple.cpp -o simple-cpp
./simple-cpp <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt
rm simple-cpp

echo C++ optimized
g++ -O2 optimized.cpp -o optimized-cpp
./optimized-cpp <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt
rm optimized-cpp

echo C simple
gcc -O2 simple.c -o simple-c
./simple-c <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt
rm simple-c

echo AWK simple
gawk -f simple.awk <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo AWK optimized
gawk -b -f optimized.awk <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt
