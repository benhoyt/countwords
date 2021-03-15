#!/usr/bin/env bash

set -e

if [ ! -f "kjvbible_x10.txt" ]; then
  for i in {1..10}; do
    cat kjvbible.txt >> kjvbible_x10.txt
  done
fi

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

echo Rust optimized
cargo build --release --manifest-path rust/optimized/Cargo.toml
./rust/optimized/target/release/countwords <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Rust optimized trie
cargo build --release --manifest-path rust/optimized-trie/Cargo.toml
./rust/optimized-trie/target/release/countwords <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Rust optimized custom hashmap
cargo build --release --manifest-path rust/optimized-customhashmap/Cargo.toml
./rust/optimized-customhashmap/target/release/countwords <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Rust bonus '(Unicode word segmentation)'
cargo build --release --manifest-path rust/bonus/Cargo.toml
./rust/bonus/target/release/countwords <kjvbible_x10.txt | python3 normalize.py >output.txt
# We don't test its output since it uses a different segmenter.

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

if [ -x ../gforth/gforth-fast ]; then
  echo Forth simple
  ../gforth/gforth-fast simple.fs <kjvbible_x10.txt | python3 normalize.py >output.txt
  git diff --exit-code output.txt

  echo Forth optimized
  ../gforth/gforth-fast optimized.fs <kjvbible_x10.txt | python3 normalize.py >output.txt
  git diff --exit-code output.txt
fi

echo Unix shell simple
bash simple.sh <kjvbible_x10.txt | awk '{ print $2, $1 }' | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Unix shell optimized
bash optimized.sh <kjvbible_x10.txt | awk '{ print $2, $1 }' | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo C# simple
csc -optimize -out:simple-cs simple.cs
chmod +x simple-cs
./simple-cs <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt
