#!/bin/sh

file="cases/if"

input=$(awk '/--- GIVEN ---/{flag=1; next} /--- EXPECT ---/{flag=0} flag' "$file")
expected=$(awk '/--- EXPECT ---/{flag=1; next} flag' "$file")

echo "$input" > "tmp"


output=$(./vslc-parser < "tmp")

echo "$output" > out
echo "$expected" > exp

diff "out" "exp"
rm "out" "exp"
