#!/bin/sh

SRCS=$(find "src" -type f -name "*.c" -not -name "parser_main.c")
OUT="metagenc"

if [ "$1" = "parser" ] 
then
    # Build standalone parser
    SRCS=$(find "src" -type f -name "*.c" -not -name "main.c")
    OUT="metagenc-parser"
fi

#CFLAGS="-Isrc -Wall -Wpedantic -Wextra -Wshadow -std=c11 -03"
CFLAGS="-Isrc -Wall -Wpedantic -Wextra -Wshadow -std=c11 -g"
#CFLAGS="-Isrc -Wall -Wpedantic -Wextra -Wshadow -std=c11 -g -fsanitize=address -fsanitize=undefined"

cc $CFLAGS $SRCS -o "$OUT"
