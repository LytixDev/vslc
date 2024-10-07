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

#include "compiler/ast.h"
#include "compiler/parser.h"

#define SAC_IMPLEMENTATION
#include "base/sac_single.h"


u32 parser(char *input)
{
    ParseResult res = parse(input);
    ParseError *parse_error = res.err_head;
    for (u32 i = 0; i < res.n_errors; i++) {
        char *msg = parse_error->msg;
        if (msg == NULL) {
            msg = PARSE_ERROR_MSGS[parse_error->type];
        }
        fprintf(stderr, "[%i] %s\n", i + 1, msg);
    }
    ast_print(res.head, res.str_list);
    printf("\n");

    free(res.str_list);

    return res.n_errors;
}


int main(void)
{
    Arena input_arena;
    m_arena_init_dynamic(&input_arena, 1, 32);
    char *input = m_arena_alloc_zero(&input_arena, 4096);
    char c;
    u32 i = 0;
    while ((c = getchar()) != EOF) {
        if (input_arena.offset > input_arena.pages_commited * 4096) {
            m_arena_alloc_zero(&input_arena, 4096);
        }
        input[i] = c;
        i++;
    }

    u32 n_errors = parser(input);
    if (n_errors == 0)
        return 0;
    return 1;
}
