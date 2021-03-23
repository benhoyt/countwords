#!/usr/bin/env bash

set -e

cat kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt kjvbible.txt >kjvbible_x10.txt

echo Python simple
python3 simple.py <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Python optimized
python3 optimized.py <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Ruby simple
ruby simple.rb <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Ruby optimized
ruby optimized.rb <kjvbible_x10.txt | python3 normalize.py >output.txt
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

echo Rust optimized-unsafe
cargo build --release --manifest-path rust/optimized-unsafe/Cargo.toml
./rust/optimized-unsafe/target/release/countwords <kjvbible_x10.txt | python3 normalize.py >output.txt
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
g++ -O2 -DNDEBUG -std=c++17 optimized.cpp -o optimized-cpp
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

echo Crystal simple
crystal build --release simple.cr -o simple-cr
./simple-cr <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo C# simple
dotnet build ./csharp/simple -c Release
./csharp/simple/bin/Release/net5.0/simple <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo C# optimized
dotnet build ./csharp/optimized -c Release
./csharp/optimized/bin/Release/net5.0/optimized <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo C# linq
dotnet build ./csharp/linq -c Release
./csharp/linq/bin/Release/net5.0/linq <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo F# simple
dotnet build ./fsharp/simple -c Release
./fsharp/simple/bin/Release/net5.0/simple <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo F# optimized
dotnet build ./fsharp/optimized -c Release
./fsharp/optimized/bin/Release/net5.0/optimized <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Swift simple
swiftc simple.swift -O -o simple-swift
./simple-swift <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Julia simple
julia simple.jl <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo nim simple
nim c -d:danger --gc:arc -o:simple-nim simple.nim
./simple-nim <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo nim optimized
nim c -d:danger --gc:arc -o:optimized-nim optimized.nim
./optimized-nim <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Perl simple
perl simple.pl <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo JavaScript simple
node ./simple <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo JavaScript optimized
node ./optimized <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo DLang simple
ldc2 -release -O3 -of=simple-d simple.d
./simple-d <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo DLang optimized
ldc2 -release -O3 -of=optimized-d optimized.d
./optimized-d <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo PHP simple
php simple.php <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo OCaml simple
ocamlopt.opt -O3 simple.ml -o simple-ml
./simple-ml <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Lua simple
luajit simple.lua <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Lua optimized
luajit optimized.lua <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Kotlin simple JVM
kotlinc simple.kt -no-reflect -include-runtime -jvm-target 14 -language-version 1.4 -d simple-kotlin.jar
java -jar simple-kotlin.jar <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Java simple
javac ./java/simple.java -d ./java
java -cp ./java simple <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Java optimized
javac ./java/optimized.java -d ./java
java -cp ./java optimized <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Zig simple
zig build-exe -OReleaseFast --name simple-zig simple.zig
./simple-zig <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Zig optimized
zig build-exe -OReleaseFast --name optimized-zig optimized.zig
./optimized-zig <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Common Lisp simple
sbcl --load simple.lisp --eval "(sb-ext:save-lisp-and-die #p\"simple-cl\" :toplevel #'main :executable t :purify t)"
./simple-cl <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Pascal simple
fpc -O3 -osimple-pas simple.pas
./simple-pas <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Tcl simple
tclsh simple.tcl <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt

echo Haxe simple
haxe -main Simple --interp <kjvbible_x10.txt | python3 normalize.py >output.txt
git diff --exit-code output.txt
