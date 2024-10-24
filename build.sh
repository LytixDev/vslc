#!/bin/sh

SRCS=$(find "src" -type f -name "*.c" -not -name "parser_main.c")
OUT="vslc"

if [ "$1" = "parser" ] 
then
    # Build standalone parser
    SRCS=$(find "src" -type f -name "*.c" -not -name "main.c")
    OUT="vslc-parser"
fi

CFLAGS="-Isrc -Wall -Wpedantic -Wextra -Wshadow -std=c11 -g"
#CFLAGS="-Isrc -Wall -Wpedantic -Wextra -Wshadow -std=c11 -g -fsanitize=address -fsanitize=undefined"

cc $CFLAGS $SRCS -o "$OUT"
