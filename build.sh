#!/bin/sh

SRCS=$(find "src" -type f -name "*.c")
CFLAGS="-Isrc -Wall -Wpedantic -Wextra -Wshadow -std=c11 -g -fsanitize=address -fsanitize=undefined -DDEBUG"
OUT="vslc"

cc $CFLAGS $SRCS -o "$OUT"
