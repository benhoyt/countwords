#!/usr/bin/env bash

set -e

echo -n '' >versions.txt

echo -n 'Linux version: ' >>versions.txt && uname -r >>versions.txt
echo >>versions.txt
python3 --version >>versions.txt
ruby --version >>versions.txt
go version >>versions.txt
rustc --version >>versions.txt
g++ --version | head -n1 >>versions.txt
gcc --version | head -n1 >>versions.txt
gawk --version | head -n1 >>versions.txt
mawk -W version 2>&1 | head -n1 >>versions.txt
bash --version | head -n1 >>versions.txt
crystal --version | head -n1 >>versions.txt
echo -n 'C#/F#/dotnet: ' >>versions.txt && dotnet --version >>versions.txt
swiftc --version | head -n1 >>versions.txt
julia --version >>versions.txt
nim --version | head -n1 >>versions.txt
perl --version | awk '$0' | head -n1 >>versions.txt
echo -n 'JavaScript/node: ' >>versions.txt && node --version >>versions.txt
ldc2 --version | head -n3 >>versions.txt
php --version | head -n1 >>versions.txt
echo -n 'OCaml: ' >>versions.txt && ocamlopt.opt --version >>versions.txt
luajit -v >>versions.txt
kotlinc -version 2>&1 | grep 'info:' >>versions.txt
javac -version >>versions.txt
echo -n 'Zig: ' >>versions.txt && zig version >>versions.txt
sbcl --version >>versions.txt

echo "For some reason gforth --version doesn't redirect! Append this to versions.txt manually:"
../gforth/gforth-fast --version 2>&1 >>versions.txt
