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
go build -o simple-go simple.go
./simple-go <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Go optimized
go build -o optimized-go optimized.go
./optimized-go <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Rust simple
cargo build --release --manifest-path rust/simple/Cargo.toml
./rust/simple/target/release/countwords <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo C++ simple
g++ -O2 simple.cpp -o simple-cpp
./simple-cpp <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo C++ optimized
g++ -O2 optimized.cpp -o optimized-cpp
./optimized-cpp <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo C simple
gcc -O2 simple.c -o simple-c
./simple-c <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo C optimized
gcc -O2 optimized.c -o optimized-c
./optimized-c <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo AWK simple
gawk -f simple.awk <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

if command -v mawk > /dev/null; then
  echo AWK optimized
  mawk -f optimized.awk <kjvbible_x10.txt | python3 normalize.py >output.txt
  git diff --exit-code output.txt
fi

if [ -x ../gforth/gforth ]; then
  echo Forth simple
  ../gforth/gforth simple.fs <kjvbible_x10.txt | python3 normalize.py >output.txt
  git diff --exit-code output.txt
fi

echo Unix shell
bash simple.sh <kjvbible_x10.txt | awk '{ print $2, $1 }' | python3 normalize.py >output.txt
git diff --exit-code output.txt
