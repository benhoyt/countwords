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
