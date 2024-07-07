/*
 *  Copyright (C) 2023-2024 Nicolai Brand (https://lytix.dev)
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <stdio.h>
#include <sys/stat.h> // TODO: should be behind an OS layer

#include "interpreter/ast.h"
#include "interpreter/parser.h"

#define SAC_IMPLEMENTATION
#include "base/sac_single.h"


int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stderr, "Expected filename argument");
        return 1;
    }

    /* Read input file */
    char *file_path = argv[1];
    struct stat st;
    if (stat(file_path, &st) != 0) {
        fprintf(stderr, "Could not stat file '%s'\n", file_path);
        return 1;
    }

    size_t input_size = st.st_size;
    char *input = malloc(sizeof(char) * (input_size + 1));
    input[input_size] = 0;
    FILE *fp = fopen(file_path, "r");
    if (fp == NULL) {
        fprintf(stderr, "Could not open file '%s'\n", file_path);
        return 1;
    }
    if (fread(input, sizeof(char), st.st_size, fp) != input_size) {
        fprintf(stderr, "Could not read file '%s'\n", file_path);
        return 1;
    }
    fclose(fp);

    AstExpr *head = parse(input);
    ast_print(head, 0);
}
