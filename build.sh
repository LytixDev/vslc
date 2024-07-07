#!/bin/sh

SRCS=$(find . -type f -name "*.c")
CFLAGS="-Isrc -Wall -Wpedantic -Wextra -Wshadow -std=c11 -g" # -fsanitize=address -fsanitize=undefined"
OUT="new-slash"

cc $CFLAGS $SRCS -o "$OUT"
