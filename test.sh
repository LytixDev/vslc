#!/bin/sh
set -e

SRCS=$(find . -type f -name "*.c" -not -name "main.c")

CFLAGS="-Isrc -Wall -Wpedantic -Wextra -Wshadow -std=c11 -g -D debug" # -fsanitize=address -fsanitize=undefined"
OUT="vslc-test"
cc $CFLAGS $SRCS -o "$OUT"

./"$OUT"
